module fpx_global
    use fpx_constants
    use fpx_logging
    use fpx_macro
    
    implicit none; private
    
    
    
    type, public :: global_t
        private
        type(macro_t), allocatable, public :: macros(:)
    end type
    
    interface global_t
        module procedure :: global_new
    end interface
    
    type(global_t), allocatable, public :: global
    
    contains
    
    type(global_t) function global_new(n) result(that)
        integer, intent(in), optional :: n
        
        if (present(n)) then
            allocate(that%macros(n))
        else
            allocate(that%macros(0))
        end if
    end function
    
end module