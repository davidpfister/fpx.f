module fpx_token
    use fpx_constants
    use fpx_macro

    implicit none; private

    public ::   evaluate_expression, &
                parse_expression

    enum, bind(c)
        enumerator :: unknown       = -1
        enumerator :: number        = 0
        enumerator :: operator      = 1
        enumerator :: identifier    = 2
        enumerator :: parenthesis   = 3
        enumerator :: defined       = 4
    end enum

    integer, parameter :: tokens_enum = kind(unknown)

    type, public :: token_t
        character(:), allocatable   :: value
        integer(tokens_enum)        :: type
    end type token_t

    contains

    logical function evaluate_expression(expr, macros) result(res)
        character(*), intent(in)    :: expr
        type(macro_t), intent(in)   :: macros(:)
        !private
        type(token_t), allocatable :: tokens(:)
        integer :: num_tokens, pos, result

        call tokenize(expr, tokens, num_tokens)
        if (num_tokens == 0) then
            print *, "No tokens found for expression"
            res = .false.
            return
        end if

        pos = 1
        result = parse_expression(tokens, num_tokens, pos, macros)
        print *, "Parsed '", trim(expr), "': pos = ", pos, ", num_tokens = ", num_tokens, ", result = ", result
        if (pos <= num_tokens) then
            print *, "Error: Extra tokens in expression: ", trim(tokens(pos)%value)
            res = .false.
            return
        end if
        res = (result /= 0)
    end function

    logical function is_digit(ch) result(res)
        character(len=1), intent(in) :: ch
        res = (ch >= '0' .and. ch <= '9')
    end function

    recursive integer function parse_expression(tokens, num_tokens, pos, macros) result(val)
        type(token_t), intent(in)   :: tokens(:)
        integer, intent(in)         :: num_tokens
        integer, intent(inout)      :: pos
        type(macro_t), intent(in)   :: macros(:)

        val = parse_or(tokens, num_tokens, pos, macros)
    end function

    recursive integer function parse_or(tokens, num_tokens, pos, macros) result(val)
        type(token_t), intent(in)   :: tokens(:)
        integer, intent(in)         :: num_tokens
        integer, intent(inout)      :: pos
        type(macro_t), intent(in)   :: macros(:)
        integer :: left

        left = parse_and(tokens, num_tokens, pos, macros)
        do while (pos <= num_tokens .and. tokens(pos)%value == '||')
            print *, "Parsing || at pos ", pos
            pos = pos + 1
            val = merge(1, 0, left /= 0 .or. parse_and(tokens, num_tokens, pos, macros) /= 0)
            left = val
        end do
        val = left
    end function

    recursive integer function parse_and(tokens, num_tokens, pos, macros) result(val)
        type(token_t), intent(in)   :: tokens(:)
        integer, intent(in)         :: num_tokens
        integer, intent(inout)      :: pos
        type(macro_t), intent(in)   :: macros(:)
        !private
        integer :: left

        left = parse_equality(tokens, num_tokens, pos, macros)
        do while (pos <= num_tokens .and. tokens(pos)%value == '&&')
            print *, "Parsing && at pos ", pos
            pos = pos + 1
            val = merge(1, 0, left /= 0 .and. parse_equality(tokens, num_tokens, pos, macros) /= 0)
            left = val
        end do
        val = left
    end function

    recursive integer function parse_equality(tokens, num_tokens, pos, macros) result(val)
        type(token_t), intent(in)   :: tokens(:)
        integer, intent(in)         :: num_tokens
        integer, intent(inout)      :: pos
        type(macro_t), intent(in)   :: macros(:)
        !private
        integer :: left, right

        left = parse_relational(tokens, num_tokens, pos, macros)
        do while (pos <= num_tokens .and. (tokens(pos)%value == '==' .or. tokens(pos)%value == '!='))
            print *, "Parsing ", trim(tokens(pos)%value), " at pos ", pos
            if (tokens(pos)%value == '==') then
                pos = pos + 1
                right = parse_relational(tokens, num_tokens, pos, macros)
                val = merge(1, 0, left == right)
            else
                pos = pos + 1
                right = parse_relational(tokens, num_tokens, pos, macros)
                val = merge(1, 0, left /= right)
            end if
            left = val
        end do
        val = left
    end function

    recursive integer function parse_relational(tokens, num_tokens, pos, macros) result(val)
        type(token_t), intent(in)   :: tokens(:)
        integer, intent(in)         :: num_tokens
        integer, intent(inout)      :: pos
        type(macro_t), intent(in)   :: macros(:)
        !private
        integer :: left, right

        left = parse_additive(tokens, num_tokens, pos, macros)
        do while (pos <= num_tokens .and. (tokens(pos)%value == '<' .or. tokens(pos)%value == '>' .or. &
                                           tokens(pos)%value == '<=' .or. tokens(pos)%value == '>='))
            print *, "Parsing ", trim(tokens(pos)%value), " at pos ", pos
            if (tokens(pos)%value == '<') then
                pos = pos + 1
                right = parse_additive(tokens, num_tokens, pos, macros)
                val = merge(1, 0, left < right)
            else if (tokens(pos)%value == '>') then
                pos = pos + 1
                right = parse_additive(tokens, num_tokens, pos, macros)
                val = merge(1, 0, left > right)
            else if (tokens(pos)%value == '<=') then
                pos = pos + 1
                right = parse_additive(tokens, num_tokens, pos, macros)
                val = merge(1, 0, left <= right)
            else
                pos = pos + 1
                right = parse_additive(tokens, num_tokens, pos, macros)
                val = merge(1, 0, left >= right)
            end if
            left = val
        end do
        val = left
    end function

    recursive integer function parse_additive(tokens, num_tokens, pos, macros) result(val)
        type(token_t), intent(in)   :: tokens(:)
        integer, intent(in)         :: num_tokens
        integer, intent(inout)      :: pos
        type(macro_t), intent(in)   :: macros(:)
        !private
        integer :: left, right

        left = parse_unary(tokens, num_tokens, pos, macros)
        do while (pos <= num_tokens .and. (tokens(pos)%value == '+' .or. tokens(pos)%value == '-'))
            print *, "Parsing ", trim(tokens(pos)%value), " at pos ", pos
            if (tokens(pos)%value == '+') then
                pos = pos + 1
                right = parse_unary(tokens, num_tokens, pos, macros)
                val = left + right
            else
                pos = pos + 1
                right = parse_unary(tokens, num_tokens, pos, macros)
                val = left - right
            end if
            left = val
        end do
        val = left
    end function

    recursive integer function parse_unary(tokens, num_tokens, pos, macros) result(val)
        type(token_t), intent(in)   :: tokens(:)
        integer, intent(in)         :: num_tokens
        integer, intent(inout)      :: pos
        type(macro_t), intent(in)   :: macros(:)

        if (pos <= num_tokens .and. tokens(pos)%value == '!') then
            print *, "Parsing ! at pos ", pos
            pos = pos + 1
            val = merge(0, 1, parse_unary(tokens, num_tokens, pos, macros) /= 0)
        else
            val = parse_primary(tokens, num_tokens, pos, macros)
        end if
    end function

    recursive integer function parse_primary(tokens, num_tokens, pos, macros) result(val)
        type(token_t), intent(in)   :: tokens(:)
        integer, intent(in)         :: num_tokens
        integer, intent(inout)      :: pos
        type(macro_t), intent(in)   :: macros(:)
        integer :: i
        character(len=MAX_LINE_LEN) :: expanded

        if (pos > num_tokens) then
            print *, "Error: Unexpected end of expression at pos ", pos
            val = 0
            return
        end if

        print *, "Parsing primary: ", trim(tokens(pos)%value), " at pos ", pos
        if (tokens(pos)%type == 0) then
            read (tokens(pos)%value, *) val
            pos = pos + 1
        else if (tokens(pos)%type == 2) then
            expanded = expand_macros(tokens(pos)%value, macros)
            read (expanded, *, iostat=i) val
            if (i /= 0) val = 0
            print *, "Expanded ", trim(tokens(pos)%value), " to ", trim(expanded), ", val = ", val
            pos = pos + 1
        else if (tokens(pos)%value == '(') then
            pos = pos + 1
            val = parse_expression(tokens, num_tokens, pos, macros)
            if (pos > num_tokens .or. tokens(pos)%value /= ')') then
                print *, "Error: Missing closing parenthesis at pos ", pos
                val = 0
            else
                print *, "Parsed ) at pos ", pos
                pos = pos + 1
            end if
        else if (tokens(pos)%type == 4) then
            expanded = trim(tokens(pos)%value)
            val = merge(1, 0, is_defined(expanded, macros))
            print *, "defined(", trim(expanded), ") = ", val
            pos = pos + 1
        else
            print *, "Error: Invalid token in expression: ", trim(tokens(pos)%value)
            val = 0
            pos = pos + 1
        end if
    end function

    subroutine tokenize(expr, tokens, num_tokens)
        character(len=*), intent(in) :: expr
        type(token_t), allocatable, intent(out) :: tokens(:)
        integer, intent(out) :: num_tokens
        character(len=MAX_LINE_LEN) :: temp
        integer :: i, pos, len_expr
        logical :: in_word

        if (allocated(tokens)) deallocate (tokens)
        allocate (tokens(MAX_TOKENS))
        num_tokens = 0
        temp = trim(adjustl(expr))
        len_expr = len_trim(temp)
        i = 1
        in_word = .false.

        do while (i <= len_expr)
            if (temp(i:i) == ' ') then
                i = i + 1
                in_word = .false.
                cycle
            end if

            if (.not. in_word) then
                num_tokens = num_tokens + 1
                if (num_tokens > MAX_TOKENS) then
                    print *, "Error: Too many tokens in expression"
                    return
                end if
                in_word = .true.
            end if

            if (temp(i:i) == '(' .or. temp(i:i) == ')') then
                tokens(num_tokens)%value = temp(i:i)
                tokens(num_tokens)%type = 3
                i = i + 1
                in_word = .false.
            else if (temp(i:i) == '!') then
                tokens(num_tokens)%value = temp(i:i)
                tokens(num_tokens)%type = 1
                i = i + 1
                in_word = .false.
            else if (temp(i:i + 1) == '&&' .or. temp(i:i + 1) == '||' .or. temp(i:i + 1) == '==' .or. &
                     temp(i:i + 1) == '!=' .or. temp(i:i + 1) == '<=' .or. temp(i:i + 1) == '>=') then
                tokens(num_tokens)%value = temp(i:i + 1)
                tokens(num_tokens)%type = 1
                i = i + 2
                in_word = .false.
            else if (temp(i:i) == '<' .or. temp(i:i) == '>' .or. temp(i:i) == '=' .or. &
                     temp(i:i) == '+' .or. temp(i:i) == '-') then
                tokens(num_tokens)%value = temp(i:i)
                tokens(num_tokens)%type = 1
                i = i + 1
                in_word = .false.
            else if (starts_with(temp(i:), 'defined')) then
                i = i + 7
                do while (i <= len_expr .and. temp(i:i) == ' ')
                    i = i + 1
                end do
                if (i <= len_expr .and. temp(i:i) == '(') then
                    i = i + 1
                    pos = i
                    do while (pos <= len_expr .and. temp(pos:pos) /= ')')
                        pos = pos + 1
                    end do
                    tokens(num_tokens)%value = trim(temp(i:pos - 1))
                    tokens(num_tokens)%type = 4
                    i = pos + 1
                else
                    pos = i
                    do while (pos <= len_expr .and. temp(pos:pos) /= ' ')
                        pos = pos + 1
                    end do
                    tokens(num_tokens)%value = trim(temp(i:pos - 1))
                    tokens(num_tokens)%type = 4
                    i = pos
                end if
                in_word = .false.
            else if (is_digit(temp(i:i))) then
                pos = i
                do while (pos <= len_expr .and. is_digit(temp(pos:pos)))
                    pos = pos + 1
                end do
                tokens(num_tokens)%value = trim(temp(i:pos - 1))
                tokens(num_tokens)%type = 0
                i = pos
                in_word = .false.
            else
                pos = i
                do while (pos <= len_expr .and. temp(pos:pos) /= ' ' .and. &
                          temp(pos:pos) /= '(' .and. temp(pos:pos) /= ')')
                    pos = pos + 1
                end do
                tokens(num_tokens)%value = trim(temp(i:pos - 1))
                tokens(num_tokens)%type = 2
                i = pos
                in_word = .false.
            end if
        end do

        print *, "Tokens for '", trim(expr), "':"
        do i = 1, num_tokens
            print *, "  Token ", i, ": ", trim(tokens(i)%value), " (type ", tokens(i)%type, ")"
        end do
    end subroutine
end module