module fpx_conditional
    use fpx_constants
    use fpx_macro, only: macro_t, is_defined
    use fpx_token, only: evaluate_expression

    implicit none; private

    public ::   handle_if,      &
                handle_ifdef,   &
                handle_ifndef,  &
                handle_elif,    &
                handle_else,    & 
                handle_endif,   &
                is_active,      &
                cond_stack,     &
                cond_depth

    type, public :: cond_state_t
        logical :: active
        logical :: has_met
    end type cond_state_t

    type(cond_state_t) :: cond_stack(MAX_COND_DEPTH)

    integer :: cond_depth = 0

    contains

    logical function is_active() result(res)
        integer :: i
        res = .true.
        do i = 1, cond_depth + 1
            if (.not. cond_stack(i)%active) then
                res = .false.
                exit
            end if
        end do
    end function

    subroutine handle_if(line, filename, line_num, macros)
        character(*), intent(in)    :: line, filename
        integer, intent(in)         :: line_num
        type(macro_t), intent(in)   :: macros(:)
        !private
        character(MAX_LINE_LEN) :: expr
        logical :: result, parent_active

        if (cond_depth + 1 > MAX_COND_DEPTH) then
            print *, "Error: Conditional nesting too deep at ", trim(filename), ":", line_num
            return
        end if

        expr = trim(adjustl(line(4:)))
        print *, "Evaluating #if: '", trim(expr), "'"
        result = evaluate_expression(expr, macros)
        parent_active = is_active()
        cond_depth = cond_depth + 1
        cond_stack(cond_depth + 1)%active = result .and. parent_active
        cond_stack(cond_depth + 1)%has_met = result
        print *, "#if result: ", result, ", cond_depth = ", cond_depth, ", active = ", cond_stack(cond_depth + 1)%active
    end subroutine

    subroutine handle_ifdef(line, filename, line_num, macros)
        character(*), intent(in)        :: line
        character(*), intent(in)        :: filename
        integer, intent(in)             :: line_num
        type(macro_t), intent(in)       :: macros(:)
        !private
        character(MAX_LINE_LEN) :: name
        logical :: defined, parent_active

        if (cond_depth + 1 > MAX_COND_DEPTH) then
            print *, "Error: Conditional nesting too deep at ", trim(filename), ":", line_num
            return
        end if

        name = trim(adjustl(line(6:)))
        defined = is_defined(name, macros)
        parent_active = is_active()
        cond_depth = cond_depth + 1
        cond_stack(cond_depth + 1)%active = defined .and. parent_active
        cond_stack(cond_depth + 1)%has_met = defined
    end subroutine

    subroutine handle_ifndef(line, filename, line_num, macros)
        character(*), intent(in)        :: line
        character(*), intent(in)        :: filename
        integer, intent(in)             :: line_num
        type(macro_t), intent(in)       :: macros(:)
        !private
        character(MAX_LINE_LEN) :: name
        logical :: defined, parent_active

        if (cond_depth + 1 > MAX_COND_DEPTH) then
            print *, "Error: Conditional nesting too deep at ", trim(filename), ":", line_num
            return
        end if

        name = trim(adjustl(line(7:)))
        defined = is_defined(name, macros)
        parent_active = is_active()
        cond_depth = cond_depth + 1
        cond_stack(cond_depth + 1)%active = (.not. defined) .and. parent_active
        cond_stack(cond_depth + 1)%has_met = .not. defined
    end subroutine

    subroutine handle_elif(line, filename, line_num, macros)
        character(*), intent(in)    :: line, filename
        integer, intent(in)         :: line_num
        type(macro_t), intent(in)   :: macros(:)
        !private
        character(MAX_LINE_LEN) :: expr
        logical :: result, parent_active

        if (cond_depth == 0) then
            print *, "Error: #elif without matching #if at ", trim(filename), ":", line_num
            return
        end if

        expr = trim(adjustl(line(5:)))
        result = evaluate_expression(expr, macros)
        parent_active = cond_depth == 0 .or. cond_stack(cond_depth)%active
        if (.not. cond_stack(cond_depth + 1)%has_met) then
            cond_stack(cond_depth + 1)%active = result .and. parent_active
            if (result) cond_stack(cond_depth + 1)%has_met = .true.
        else
            cond_stack(cond_depth + 1)%active = .false.
        end if
    end subroutine

    subroutine handle_else(filename, line_num)
        character(*), intent(in) :: filename
        integer, intent(in) :: line_num
        logical :: parent_active

        if (cond_depth == 0) then
            print *, "Error: #else without matching #if at ", trim(filename), ":", line_num
            return
        end if

        parent_active = cond_depth == 0 .or. cond_stack(cond_depth)%active
        if (.not. cond_stack(cond_depth + 1)%has_met) then
            cond_stack(cond_depth + 1)%active = parent_active
            cond_stack(cond_depth + 1)%has_met = .true.
        else
            cond_stack(cond_depth + 1)%active = .false.
        end if
        print *, "#else at depth ", cond_depth, ", active = ", cond_stack(cond_depth + 1)%active
    end subroutine

    subroutine handle_endif(filename, line_num)
        character(*), intent(in) :: filename
        integer, intent(in) :: line_num

        if (cond_depth == 0) then
            print *, "Error: #endif without matching #if at ", trim(filename), ":", line_num
            return
        end if

        print *, "#endif at depth ", cond_depth
        cond_depth = cond_depth - 1
    end subroutine

end module
