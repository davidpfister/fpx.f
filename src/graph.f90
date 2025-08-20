module fpx_graph
    implicit none; private
    
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
    
    interface digraph
        module procedure :: graph_new
    end interface

contains

    type(digraph) function graph_new(vertices) result(that)
        integer, intent(in) :: vertices
        integer :: i

        that%vertices = vertices
        allocate(that%adjacency_list(vertices, vertices), source=0)
        allocate(that%list_sizes(vertices), source=0)
    end function

    subroutine graph_add_edge(this, source, destination)
        class(digraph), intent(inout) :: this
        integer, intent(in) :: source, destination
        
        if (source < 1 .or. source > this%vertices .or. &
            destination < 1 .or. destination > this%vertices) then
            return ! Skip invalid edges
        end if
        
        this%list_sizes(source) = this%list_sizes(source) + 1
        if (this%list_sizes(source) <= this%vertices) then
            this%adjacency_list(source, this%list_sizes(source)) = destination
        end if
    end subroutine

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

    subroutine graph_final(this)
        type(digraph), intent(inout) :: this
        if (allocated(this%adjacency_list)) deallocate(this%adjacency_list)
        if (allocated(this%list_sizes)) deallocate(this%list_sizes)
    end subroutine

end module