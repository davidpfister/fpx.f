!> @brief Lightweight directed graph implementation for cycle detection in macro expansion
!!
!! This module provides a compact, allocation-efficient directed graph (`digraph`) specifically
!! designed for detecting circular dependencies during macro expansion in the fpx preprocessor.
!!
!! Key features:
!! - Fixed-size adjacency list using a dense 2D integer array (fast access, no pointers)
!! - Dynamic edge insertion with automatic per-vertex size tracking
!! - Depth-first search (DFS) based) cycle detection starting from any vertex
!! - Automatic cleanup via finalizer
!! - No dynamic memory fragmentation – ideal for frequent creation/destruction during preprocessing
!!
!! Used internally by `fpx_macro` to prevent infinite recursion when a macro expands
!! (directly or indirectly) to itself (e.g., `#define A B`, `#define B A`).
!!
!! @par Examples
!!
!! 1. Detect circular macro dependency:
!! @code{.f90}
!!    type(digraph) :: g
!!    logical       :: cycle
!!
!!    g = digraph(3)            ! 3 macros indexed 1..3
!!    call g%add_edge(1, 2)     ! macro1 depends on macro2
!!    call g%add_edge(2, 3)     ! macro2 depends on macro3
!!    call g%add_edge(3, 1)     ! macro3 depends on macro1 → cycle!
!!
!!    cycle = g%is_circular(1)  ! returns .true.
!!    print *, "Circular macro chain detected:", cycle
!! @endcode
!!
!! 2. Safe expansion (used inside fpx_macro):
!! @code{.f90}
!!    type(digraph) :: expansion_graph
!!    expansion_graph = digraph(size(macros))
!!    call expansion_graph%add_edge(current_macro_idx, referenced_macro_idx)
!!    if (expansion_graph%is_circular(referenced_macro_idx)) then
!!       ! Skip expansion to prevent infinite loop
!!    end if
!! @endcode
!!
!! The graph is automatically deallocated when it goes out of scope thanks to the finalizer.
module fpx_graph
    implicit none; private
    
    !> @brief Directed graph with fixed vertex count and efficient cycle detection
    !! Stores edges in a dense adjacency matrix slice per vertex.
    !! Only the actually used portion of each row is tracked via `list_sizes`.
    type, public :: digraph
        private
        integer :: vertices
        integer, allocatable :: adjacency_list(:,:)
        integer, allocatable :: list_sizes(:)
    contains
        private
        procedure, pass(this), public :: add_edge => graph_add_edge
        procedure, pass(this), public :: is_circular => graph_has_cycle_dfs
        final :: graph_final
    end type
    
    !> @brief Constructor interface for digraph
    interface digraph
        module procedure :: graph_new
    end interface

contains

    !> @brief Construct a new directed graph with given number of vertices
    !! @param[in] vertices Number of vertices (usually number of currently defined macros)
    !! @return Initialized digraph ready for edge insertion
    type(digraph) function graph_new(vertices) result(that)
        integer, intent(in) :: vertices
        integer :: i

        that%vertices = vertices
        allocate(that%adjacency_list(vertices, vertices), source=0)
        allocate(that%list_sizes(vertices), source=0)
    end function

    !> @brief Add a directed edge from source → destination
    !! Silently ignores invalid indices. Optional `exists` flag indicates if edge was already present.
    !! @param[in]    source      Source vertex (1-based)
    !! @param[in]    destination Target vertex (1-based)
    !! @param[out]   exists      (optional) .true. if edge already existed
    subroutine graph_add_edge(this, source, destination, exists)
        class(digraph), intent(inout)   :: this
        integer, intent(in)             :: source
        integer, intent(in)             :: destination
        logical, intent(out), optional  :: exists
        
        if (source < 1 .or. source > this%vertices .or. &
            destination < 1 .or. destination > this%vertices) then
            return ! Skip invalid edges
        end if
        
        this%list_sizes(source) = this%list_sizes(source) + 1
        if (this%list_sizes(source) <= this%vertices) then
            if (present(exists)) exists = this%adjacency_list(source, this%list_sizes(source)) /= 0
            this%adjacency_list(source, this%list_sizes(source)) = destination
        end if
    end subroutine

    !> @brief Check whether a cycle exists in the graph reachable from start_vertex
    !! Uses standard DFS with recursion stack (back-edge detection).
    !! @param[in] start_vertex Vertex from which to begin cycle search
    !! @return .true. if a cycle is found in the component reachable from start_vertex
    logical function graph_has_cycle_dfs(this, start_vertex) result(has_cycle)
        class(digraph), intent(in) :: this
        integer, intent(in) :: start_vertex
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

    !> @brief Internal recursive DFS worker for cycle detection
    recursive logical function dfs_recursive(this, vertex, visited, recursion_stack) result(has_cycle)
        class(digraph), intent(in) :: this
        integer, intent(in) :: vertex
        logical, intent(inout) :: visited(:), recursion_stack(:)
        integer :: neighbor, i

        visited(vertex) = .true.
        recursion_stack(vertex) = .true.

        do i = 1, this%list_sizes(vertex)
            neighbor = this%adjacency_list(vertex, i)
            if (neighbor < 1 .or. neighbor > this%vertices) cycle ! Skip invalid neighbors
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

    !> @brief Finalizer – automatically deallocate internal arrays when graph goes out of scope
    subroutine graph_final(this)
        type(digraph), intent(inout) :: this
        if (allocated(this%adjacency_list)) deallocate(this%adjacency_list)
        if (allocated(this%list_sizes)) deallocate(this%list_sizes)
    end subroutine

end module