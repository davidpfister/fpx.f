module fpx_conditional
    use fpx_constants
    use fpx_macro
    use fpx_token
    use fpx_stack

    implicit none; private

    public ::   handle_if,      &
                handle_ifdef,   &
                handle_ifndef,  &
                handle_elif,    &
                handle_else,    & 
                handle_endif,   &
                cond_stack,     &
                cond_depth

    contains

    subroutine handle_if(line, filename, line_num)
        character(len=*), intent(in) :: line, filename
        integer, intent(in) :: line_num
        character(len=MAX_LINE_LEN) :: expr
        logical :: result, parent_active

        if (cond_depth + 1 > MAX_COND_DEPTH) then
            print *, "Error: Conditional nesting too deep at ", trim(filename), ":", line_num
            return
        end if

        expr = trim(adjustl(line(4:)))
        print *, "Evaluating #if: '", trim(expr), "'"
        result = evaluate_expression(expr)
        parent_active = is_active()
        cond_depth = cond_depth + 1
        cond_stack(cond_depth + 1)%active = result .and. parent_active
        cond_stack(cond_depth + 1)%has_met = result
        print *, "#if result: ", result, ", cond_depth = ", cond_depth, ", active = ", cond_stack(cond_depth + 1)%active
    end subroutine

    subroutine handle_ifdef(line, filename, line_num)
        character(len=*), intent(in) :: line, filename
        integer, intent(in) :: line_num
        character(len=MAX_LINE_LEN) :: name
        logical :: defined, parent_active

        if (cond_depth + 1 > MAX_COND_DEPTH) then
            print *, "Error: Conditional nesting too deep at ", trim(filename), ":", line_num
            return
        end if

        name = trim(adjustl(line(6:)))
        defined = is_defined(name)
        parent_active = is_active()
        cond_depth = cond_depth + 1
        cond_stack(cond_depth + 1)%active = defined .and. parent_active
        cond_stack(cond_depth + 1)%has_met = defined
    end subroutine

    subroutine handle_ifndef(line, filename, line_num)
        character(len=*), intent(in) :: line, filename
        integer, intent(in) :: line_num
        character(len=MAX_LINE_LEN) :: name
        logical :: defined, parent_active

        if (cond_depth + 1 > MAX_COND_DEPTH) then
            print *, "Error: Conditional nesting too deep at ", trim(filename), ":", line_num
            return
        end if

        name = trim(adjustl(line(7:)))
        defined = is_defined(name)
        parent_active = is_active()
        cond_depth = cond_depth + 1
        cond_stack(cond_depth + 1)%active = (.not. defined) .and. parent_active
        cond_stack(cond_depth + 1)%has_met = .not. defined
    end subroutine

    subroutine handle_elif(line, filename, line_num)
        character(len=*), intent(in) :: line, filename
        integer, intent(in) :: line_num
        character(len=MAX_LINE_LEN) :: expr
        logical :: result, parent_active

        if (cond_depth == 0) then
            print *, "Error: #elif without matching #if at ", trim(filename), ":", line_num
            return
        end if

        expr = trim(adjustl(line(5:)))
        result = evaluate_expression(expr)
        parent_active = cond_depth == 0 .or. cond_stack(cond_depth)%active
        if (.not. cond_stack(cond_depth + 1)%has_met) then
            cond_stack(cond_depth + 1)%active = result .and. parent_active
            if (result) cond_stack(cond_depth + 1)%has_met = .true.
        else
            cond_stack(cond_depth + 1)%active = .false.
        end if
    end subroutine

    subroutine handle_else(filename, line_num)
        character(len=*), intent(in) :: filename
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
        character(len=*), intent(in) :: filename
        integer, intent(in) :: line_num

        if (cond_depth == 0) then
            print *, "Error: #endif without matching #if at ", trim(filename), ":", line_num
            return
        end if

        print *, "#endif at depth ", cond_depth
        cond_depth = cond_depth - 1
    end subroutine

    logical function evaluate_expression(expr) result(res)
        character(len=*), intent(in) :: expr
        type(token_t), allocatable :: tokens(:)
        integer :: num_tokens, pos, result

        call tokenize(expr, tokens, num_tokens)
        if (num_tokens == 0) then
            print *, "No tokens found for expression"
            res = .false.
            return
        end if

        pos = 1
        result = parse_expression(tokens, num_tokens, pos)
        print *, "Parsed '", trim(expr), "': pos = ", pos, ", num_tokens = ", num_tokens, ", result = ", result
        if (pos <= num_tokens) then
            print *, "Error: Extra tokens in expression: ", trim(tokens(pos)%value)
            res = .false.
            return
        end if
        res = (result /= 0)
    end function
end module
