module fpx_conditional
    use fpx_constants
    use fpx_logging
    use fpx_macro, only: macro_t, is_defined
    use fpx_token, only: evaluate_expression

    implicit none; private

    public :: handle_if, &
              handle_ifdef, &
              handle_ifndef, &
              handle_elif, &
              handle_else, &
              handle_endif, &
              is_active, &
              cond_stack, &
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
        integer :: pos

        if (cond_depth + 1 > MAX_COND_DEPTH) then
            if (verbose) print *, "Error: Conditional nesting too deep at ", trim(filename), ":", line_num
            return
        end if

        pos = index(line, 'if') + len('if')
        expr = trim(adjustl(line(pos:)))
        if (verbose) print *, "Evaluating #if: '", trim(expr), "'"
        result = evaluate_expression(expr, macros)
        parent_active = is_active()
        cond_depth = cond_depth + 1
        cond_stack(cond_depth + 1)%active = result .and. parent_active
        cond_stack(cond_depth + 1)%has_met = result
        if (verbose) print *, "#if result: ", result, ", cond_depth = ", cond_depth, ", active = ", cond_stack(cond_depth + 1)%active
    end subroutine

    subroutine handle_ifdef(line, filename, line_num, macros)
        character(*), intent(in)        :: line
        character(*), intent(in)        :: filename
        integer, intent(in)             :: line_num
        type(macro_t), intent(in)       :: macros(:)
        !private
        character(MAX_LINE_LEN) :: name
        logical :: defined, parent_active
        integer :: pos

        if (cond_depth + 1 > MAX_COND_DEPTH) then
            if (verbose) print *, "Error: Conditional nesting too deep at ", trim(filename), ":", line_num
            return
        end if

        pos = index(line, 'ifdef') + len('ifdef')
        name = trim(adjustl(line(pos:)))
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
        integer :: pos

        if (cond_depth + 1 > MAX_COND_DEPTH) then
            if (verbose) print *, "Error: Conditional nesting too deep at ", trim(filename), ":", line_num
            return
        end if

        pos = index(line, 'ifndef') + len('ifndef')
        name = trim(adjustl(line(pos:)))
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
        integer :: pos

        if (cond_depth == 0) then
            if (verbose) print *, "Error: #elif without matching #if at ", trim(filename), ":", line_num
            return
        end if

        pos = index(line, 'elif') + len('elif')
        expr = trim(adjustl(line(pos:)))
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
            if (verbose) print *, "Error: #else without matching #if at ", trim(filename), ":", line_num
            return
        end if

        parent_active = cond_depth == 0 .or. cond_stack(cond_depth)%active
        if (.not. cond_stack(cond_depth + 1)%has_met) then
            cond_stack(cond_depth + 1)%active = parent_active
            cond_stack(cond_depth + 1)%has_met = .true.
        else
            cond_stack(cond_depth + 1)%active = .false.
        end if
        if (verbose) print *, "#else at depth ", cond_depth, ", active = ", cond_stack(cond_depth + 1)%active
    end subroutine

    subroutine handle_endif(filename, line_num)
        character(*), intent(in) :: filename
        integer, intent(in) :: line_num

        if (cond_depth == 0) then
            if (verbose) print *, "Error: #endif without matching #if at ", trim(filename), ":", line_num
            return
        end if

        if (verbose) print *, "#endif at depth ", cond_depth
        cond_depth = cond_depth - 1
    end subroutine

end module
