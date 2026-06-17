!> @file
!! @defgroup group_graph Graph
!! Directed graph utilities used for macro dependency analysis.
!!
!! This module provides a lightweight directed graph implementation used
!! internally by the FPX preprocessor to detect cyclic dependencies during
!! macro expansion.
!!
!! Unlike general-purpose graph libraries, this implementation is optimized
!! for the small graphs typically encountered during preprocessing:
!!
!! - Vertices are represented by 1-based integer identifiers.
!! - Edges are stored in a dense adjacency structure for fast traversal.
!! - Cycle detection uses depth-first search (DFS) with a recursion stack.
!! - Invalid vertices are ignored gracefully.
!! - Memory management is automatic through a finalizer.
!!
!! The primary use case is preventing infinite recursion caused by macros
!! expanding, directly or indirectly, to themselves:
!!
!! @code{.f90}
!!    #define A B
!!    #define B C
!!    #define C A
!! ...
!! @endcode
!!
!! Before expanding a macro, FPX records dependencies in a graph and checks
!! whether introducing a new dependency would create a cycle.
!!
!! @section graph_examples Examples
!!
!! 1. Detecting a circular dependency:
!! @code{.f90}
!!    type(digraph) :: g
!!    logical       :: cycle
!!
!!    g = digraph(3)
!!
!!    call g%add_edge(1, 2)
!!    call g%add_edge(2, 3)
!!    call g%add_edge(3, 1)
!!
!!    cycle = g%is_circular(1)
!!    print *, cycle      ! prints .true.
!! ...
!! @endcode
!!
!! 2. Detecting an acyclic dependency chain:
!! @code{.f90}
!!    type(digraph) :: g
!!
!!    g = digraph(4)
!!
!!    call g%add_edge(1, 2)
!!    call g%add_edge(2, 3)
!!    call g%add_edge(3, 4)
!!
!!    print *, g%is_circular(1)   ! .false.
!! ...
!! @endcode
!!
!! 3. Internal usage during macro expansion:
!! @code{.f90}
!!    call graph%add_edge(current_macro, referenced_macro)
!!
!!    if (graph%is_circular(referenced_macro)) then
!!        ! Prevent recursive expansion
!!    end if
!! ...
!! @endcode
module fpx_graph
    implicit none; private

    !> Directed graph supporting efficient cycle detection.
    !!
    !! The graph stores a fixed number of vertices identified by
    !! integers in the range `[1, vertices]`.
    !!
    !! Edges are represented internally using a dense adjacency
    !! structure together with per-vertex occupancy counters.
    !! This approach avoids repeated allocations and is well suited
    !! to the relatively small dependency graphs encountered by fpx.
    !!
    !! @section digraph_type_examples Examples
    !! @code{.f90}
    !!    type(digraph) :: g
    !!
    !!    g = digraph(2)
    !!    call g%add_edge(1, 2)
    !!
    !!    print *, g%is_circular(1)
    !! ...
    !! @endcode
    !!
    !! @section digraph_type_constructors Constructors
    !! Initializes a new directed graph.
    !!
    !! @b Constructor
    !! @code{.f90}
    !! type(digraph) function digraph(integer vertices)
    !! @endcode
    !!
    !! @param[in] vertices 
    !!   Number of vertices in the graph.
    !!
    !! @return A newly constructed directed graph.
    !!
    !! @section digraph_type_remarks Remarks
    !! - Vertices are numbered from 1.
    !! - The number of vertices is fixed after construction.
    !! - Intended primarily for internal use by the macro expander.
    !!
    !! @ingroup group_graph
    type, public :: digraph
        integer, private :: vertices  !< Number of vertices
        integer, allocatable, private :: adjacency_list(:, :)  !< Adjacency list containing the connection information between the vertices.
        integer, allocatable, private :: list_sizes(:)  !< Actually used portion of each row of @ref adjacency_list.
    contains
        private
        procedure, pass(this), public :: add_edge => graph_add_edge
        procedure, pass(this), public :: is_circular => graph_has_cycle_dfs
        final :: graph_final
    end type

    !> Construct a directed graph with a fixed number of vertices.
    !!
    !! Allocates the internal adjacency structures and initializes
    !! the graph without any edges.
    !!
    !! @param[in] vertices 
    !!   Number of vertices.
    !!
    !! @return Newly initialized graph.
    !!
    !! @ingroup group_graph
    interface digraph
        !! @cond
        module procedure :: graph_new
        !! @endcond
    end interface

contains

    type(digraph) function graph_new(vertices) result(that)
        integer, intent(in) :: vertices
        integer :: i

        that%vertices = vertices
        allocate(that%adjacency_list(vertices, vertices), source=0)
        allocate(that%list_sizes(vertices), source=0)
    end function

    !> Add a directed edge to the graph.
    !!
    !! Inserts an edge from `source` to `destination`.
    !! If either vertex lies outside the valid range,
    !! the request is ignored.
    !!
    !! @param[inout] this        Graph instance.
    !! @param[in]    source      Source vertex (1-based).
    !! @param[in]    destination Destination vertex (1-based).
    !! @param[out]   overflow    Optional flag indicating whether the
    !!                           insertion position was already occupied.
    !!
    !! @note Duplicate edges are not explicitly filtered.
    !!
    !! @ingroup group_graph
    subroutine graph_add_edge(this, source, destination, overflow)
        class(digraph), intent(inout)   :: this
        integer, intent(in)             :: source
        integer, intent(in)             :: destination
        logical, intent(out), optional  :: overflow

        if (source < 1 .or. source > this%vertices .or. &
                destination < 1 .or. destination > this%vertices) then
            return  ! Skip invalid edges
        end if

        this%list_sizes(source) = this%list_sizes(source) + 1
        if (this%list_sizes(source) <= this%vertices) then
            if (present(overflow)) overflow = this%adjacency_list(source, this%list_sizes(source)) /= 0
            this%adjacency_list(source, this%list_sizes(source)) = destination
        end if
    end subroutine

    !> Determine whether a cycle is reachable from a vertex.
    !!
    !! Performs a depth-first traversal starting from
    !! `start_vertex` and detects back edges using a
    !! recursion stack.
    !!
    !! @param[in] this
    !!   Graph instance.
    !! @param[in] start_vertex
    !!   Vertex from which the search begins.
    !!
    !! @return `.true.` if a cycle exists in the reachable component;
    !!         `.false.` otherwise.
    !!
    !! @ingroup group_graph
    logical function graph_has_cycle_dfs(this, start_vertex) result(has_cycle)
        class(digraph), intent(in) :: this
        integer, intent(in) :: start_vertex
        !private
        logical, allocatable :: visited(:), recursion_stack(:)

        if (start_vertex < 1 .or. start_vertex > this%vertices) then
            has_cycle = .false.
            return
        end if

        allocate(visited(this%vertices), source=.false.)
        allocate(recursion_stack(this%vertices), source=.false.)

        has_cycle = dfs_recursive(this, start_vertex, visited, recursion_stack)

        deallocate(visited, recursion_stack)
    end function

    !> Recursive DFS worker used for cycle detection.
    !!
    !! This routine implements the actual traversal algorithm used
    !! by @ref graph_has_cycle_dfs. It maintains both a visited set
    !! and a recursion stack in order to identify back edges.
    !!
    !! @ingroup group_graph
    recursive logical function dfs_recursive(this, vertex, visited, recursion_stack) result(has_cycle)
        class(digraph), intent(in) :: this
        integer, intent(in) :: vertex
        logical, intent(inout) :: visited(:), recursion_stack(:)
        integer :: neighbor, i

        visited(vertex) = .true.
        recursion_stack(vertex) = .true.

        do i = 1, this%list_sizes(vertex)
            neighbor = this%adjacency_list(vertex, i)
            if (neighbor < 1 .or. neighbor > this%vertices) cycle  ! Skip invalid neighbors
            if (.not. visited(neighbor)) then
                if (dfs_recursive(this, neighbor, visited, recursion_stack)) then
                    has_cycle = .true.
                    return
                end if
            else if (recursion_stack(neighbor)) then
                has_cycle = .true.
                return
            end if
        end do

        recursion_stack(vertex) = .false.
        has_cycle = .false.
    end function

    !> Finalizer for the directed graph.
    !!
    !! Releases all dynamically allocated storage associated with
    !! the graph when it leaves scope.
    !!
    !! @ingroup group_graph
    subroutine graph_final(this)
        type(digraph), intent(inout) :: this
        if (allocated(this%adjacency_list)) deallocate(this%adjacency_list)
        if (allocated(this%list_sizes)) deallocate(this%list_sizes)
    end subroutine

end module
