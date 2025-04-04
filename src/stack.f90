module fpx_stack
    use fpx_constants
    
    implicit none; private

    public ::   cond_stack,     &
                cond_depth

    type, public :: cond_state_t
        logical :: active
        logical :: has_met
    end type cond_state_t

    type(cond_state_t) :: cond_stack(MAX_COND_DEPTH)

    integer :: cond_depth = 0

end module