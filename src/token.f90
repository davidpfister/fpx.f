module fpx_token
    use fpx_string
    use fpx_constants
    use fpx_macro
    use fpx_logging

    implicit none; private

    public ::   evaluate_expression, &
                parse_expression

    enum, bind(c)
        enumerator :: unknown = -1
        enumerator :: number = 0
        enumerator :: operator = 1
        enumerator :: identifier = 2
        enumerator :: parenthesis = 3
        enumerator :: defined = 4
    end enum

    integer, parameter :: tokens_enum = kind(unknown)

    type, public :: token
        character(:), allocatable   :: value
        integer(tokens_enum)        :: type
    end type
    
    interface strtol
        module procedure :: strtol_default
        module procedure :: strtol_with_base
    end interface

contains

    logical function evaluate_expression(expr, macros, val) result(res)
        character(*), intent(in)        :: expr
        type(macro), intent(in)         :: macros(:)
        integer, intent(out), optional  :: val
        !private
        type(token), allocatable :: tokens(:)
        integer :: ntokens, pos, result

        call tokenize(expr, tokens, ntokens)
        if (ntokens == 0) then
            if (verbose) print *, "No tokens found for expression"
            res = .false.
            return
        end if

        pos = 1
        result = parse_expression(tokens, ntokens, pos, macros)
        if (verbose) print *, "Parsed '", trim(expr), "': pos = ", pos, ", ntokens = ", ntokens, ", result = ", result
        if (pos <= ntokens) then
            if (verbose) print *, "Error: Extra tokens in expression: ", trim(tokens(pos)%value)
            res = .false.
            return
        end if
        res = (result /= 0)
        if (present(val)) val = result
    end function

    logical elemental function is_digit(ch) result(res)
        character(*), intent(in) :: ch
        
        res = verify(ch, '0123456789') == 0
    end function
    
    logical function is_typeless(str, pos) result(res)
        character(*), intent(in)    :: str
        integer, intent(out)        :: pos
        !private
        integer :: i, base, n
        
        pos = 0; base = 0; n = len(str)
        do i = 1, n
            if (verify(str(i:i), '0123456789xXaAbBcCdDeEfF') /= 0) then
                pos = i
                exit
            end if
        end do
        if (pos > 0) i = strtol(str(:pos - 1), base, success = res)
        if (base == 10) res = .false.
    end function
    
    integer function strtol_default(str, success) result(val)
        character(*), intent(in)        :: str
        logical, intent(out), optional  :: success
        !private
        integer :: base
        
        base = 0
        val = strtol_with_base(str, base, success)
    end function
    
    integer function strtol_with_base(str, base, success) result(val)
        character(*), intent(in)        :: str
        integer, intent(inout)          :: base
        logical, intent(out), optional  :: success
        !private
        integer :: i, len, digit
        character :: c
        logical :: is_valid, isdigit, is_lower_hex, is_upper_hex
        character(len=len_trim(str)) :: work_str

        val = 0; is_valid = .true.
        work_str = adjustl(str) ! Remove leading spaces
        len = len_trim(work_str)

        ! Handle base 0 (auto-detect)
        if (base == 0) then
            if (len >= 2) then
                if (work_str(1:2) == '0x' .or. work_str(1:2) == '0X') then
                    base = 16
                    work_str = work_str(3:len)
                    len = len - 2
                else if (work_str(1:2) == '0b' .or. work_str(1:2) == '0B') then
                    base = 2
                    work_str = work_str(3:len)
                    len = len - 2
                else
                    if (len > 1) then
                        if (work_str(1:1) == '0') then
                            base = 8
                        else
                            base = 10
                        end if
                    else
                        base = 10
                    end if
                end if
            else
                base = 10
            end if
        end if

        ! Validate base
        if (base /= 2 .and. base /= 8 .and. base /= 10 .and. base /= 16) then
            is_valid = .false.
            if (present(success)) success = .false.
            return
        end if

        ! Process each character
        do i = 1, len
            c = work_str(i:i)
            digit = -1 ! Invalid digit marker

            ! Convert character to digit
            isdigit = c >= '0' .and. c <= '9'
            if (isdigit) digit = ichar(c) - ichar('0')

            is_lower_hex = base == 16 .and. c >= 'a' .and. c <= 'f'
            if (is_lower_hex) digit = ichar(c) - ichar('a') + 10

            is_upper_hex = base == 16 .and. c >= 'A' .and. c <= 'F'
            if (is_upper_hex) digit = ichar(c) - ichar('A') + 10

            ! Check if digit is valid
            if (digit == -1) then
                is_valid = .false.
                exit
            end if
            if (digit >= base) then
                is_valid = .false.
                exit
            end if

            ! Check for potential overflow (approximate for 32-bit integer)
            if (val > (huge(val) - digit) / base) then
                is_valid = .false.
                exit
            end if

            ! Accumulate value
            val = val * base + digit
        end do

        ! Set success flag if provided
        if (present(success)) success = is_valid
    end function

    recursive integer function parse_expression(tokens, ntokens, pos, macros) result(val)
        type(token), intent(in)   :: tokens(:)
        integer, intent(in)         :: ntokens
        integer, intent(inout)      :: pos
        type(macro), intent(in)   :: macros(:)

        val = parse_or(tokens, ntokens, pos, macros)
    end function

    recursive integer function parse_or(tokens, ntokens, pos, macros) result(val)
        type(token), intent(in)   :: tokens(:)
        integer, intent(in)         :: ntokens
        integer, intent(inout)      :: pos
        type(macro), intent(in)   :: macros(:)
        integer :: left

        left = parse_and(tokens, ntokens, pos, macros)
        do while (pos <= ntokens .and. tokens(pos)%value == '||')
            if (verbose) print *, "Parsing || at pos ", pos
            pos = pos + 1
            val = merge(1, 0, left /= 0 .or. parse_and(tokens, ntokens, pos, macros) /= 0)
            left = val
        end do
        val = left
    end function

    recursive integer function parse_and(tokens, ntokens, pos, macros) result(val)
        type(token), intent(in)   :: tokens(:)
        integer, intent(in)         :: ntokens
        integer, intent(inout)      :: pos
        type(macro), intent(in)   :: macros(:)
        !private
        integer :: left

        left = parse_bitwise_or(tokens, ntokens, pos, macros)
        do while (pos <= ntokens .and. tokens(pos)%value == '&&')
            if (verbose) print *, "Parsing && at pos ", pos
            pos = pos + 1
            val = merge(1, 0, left /= 0 .and. parse_bitwise_or(tokens, ntokens, pos, macros) /= 0)
            left = val
        end do
        val = left
    end function
    
    recursive integer function parse_bitwise_or(tokens, ntokens, pos, macros) result(val)
        type(token), intent(in)   :: tokens(:)
        integer, intent(in)         :: ntokens
        integer, intent(inout)      :: pos
        type(macro), intent(in)   :: macros(:)
        !private
        integer :: left

        left = parse_bitwise_xor(tokens, ntokens, pos, macros)
        do while (pos <= ntokens .and. tokens(pos)%value == '|')
            if (verbose) print *, "Parsing && at pos ", pos
            pos = pos + 1
            val = parse_bitwise_xor(tokens, ntokens, pos, macros)
            left = ior(left, val)
        end do
        val = left
    end function
    
    recursive integer function parse_bitwise_xor(tokens, ntokens, pos, macros) result(val)
        type(token), intent(in)   :: tokens(:)
        integer, intent(in)         :: ntokens
        integer, intent(inout)      :: pos
        type(macro), intent(in)   :: macros(:)
        !private
        integer :: left

        left = parse_bitwise_and(tokens, ntokens, pos, macros)
        do while (pos <= ntokens .and. tokens(pos)%value == '^')
            if (verbose) print *, "Parsing ^ at pos ", pos
            pos = pos + 1
            val = parse_bitwise_and(tokens, ntokens, pos, macros)
            left = ieor(left, val)
        end do
        val = left
    end function
    
    recursive integer function parse_bitwise_and(tokens, ntokens, pos, macros) result(val)
        type(token), intent(in)   :: tokens(:)
        integer, intent(in)         :: ntokens
        integer, intent(inout)      :: pos
        type(macro), intent(in)   :: macros(:)
        !private
        integer :: left

        left = parse_equality(tokens, ntokens, pos, macros)
        do while (pos <= ntokens .and. tokens(pos)%value == '&')
            if (verbose) print *, "Parsing && at pos ", pos
            pos = pos + 1
            val = parse_equality(tokens, ntokens, pos, macros)
            left = iand(left, val)
        end do
        val = left
    end function

    recursive integer function parse_equality(tokens, ntokens, pos, macros) result(val)
        type(token), intent(in)   :: tokens(:)
        integer, intent(in)         :: ntokens
        integer, intent(inout)      :: pos
        type(macro), intent(in)   :: macros(:)
        !private
        integer :: left, right

        left = parse_relational(tokens, ntokens, pos, macros)
        do while (pos <= ntokens .and. (tokens(pos)%value == '==' .or. tokens(pos)%value == '!='))
            if (verbose) print *, "Parsing ", trim(tokens(pos)%value), " at pos ", pos
            if (tokens(pos)%value == '==') then
                pos = pos + 1
                right = parse_relational(tokens, ntokens, pos, macros)
                val = merge(1, 0, left == right)
            else
                pos = pos + 1
                right = parse_relational(tokens, ntokens, pos, macros)
                val = merge(1, 0, left /= right)
            end if
            left = val
        end do
        val = left
    end function

    recursive integer function parse_relational(tokens, ntokens, pos, macros) result(val)
        type(token), intent(in)   :: tokens(:)
        integer, intent(in)         :: ntokens
        integer, intent(inout)      :: pos
        type(macro), intent(in)   :: macros(:)
        !private
        integer :: left, right

        left = parse_shifting(tokens, ntokens, pos, macros)
        do while (pos <= ntokens .and. (tokens(pos)%value == '<' .or. tokens(pos)%value == '>' .or. &
                                           tokens(pos)%value == '<=' .or. tokens(pos)%value == '>='))
            if (verbose) print *, "Parsing ", trim(tokens(pos)%value), " at pos ", pos
            if (tokens(pos)%value == '<') then
                pos = pos + 1
                right = parse_shifting(tokens, ntokens, pos, macros)
                val = merge(1, 0, left < right)
            else if (tokens(pos)%value == '>') then
                pos = pos + 1
                right = parse_shifting(tokens, ntokens, pos, macros)
                val = merge(1, 0, left > right)
            else if (tokens(pos)%value == '<=') then
                pos = pos + 1
                right = parse_shifting(tokens, ntokens, pos, macros)
                val = merge(1, 0, left <= right)
            else
                pos = pos + 1
                right = parse_shifting(tokens, ntokens, pos, macros)
                val = merge(1, 0, left >= right)
            end if
            left = val
        end do
        val = left
    end function
    
    recursive integer function parse_shifting(tokens, ntokens, pos, macros) result(val)
        type(token), intent(in)   :: tokens(:)
        integer, intent(in)         :: ntokens
        integer, intent(inout)      :: pos
        type(macro), intent(in)   :: macros(:)
        !private
        integer :: left, right

        left = parse_additive(tokens, ntokens, pos, macros)
        do while (pos <= ntokens .and. (tokens(pos)%value == '<<' .or. tokens(pos)%value == '>>'))
            if (verbose) print *, "Parsing ", trim(tokens(pos)%value), " at pos ", pos
            if (tokens(pos)%value == '<<') then
                pos = pos + 1
                right = parse_additive(tokens, ntokens, pos, macros)
                val = lshift(left, right)
            else
                pos = pos + 1
                right = parse_additive(tokens, ntokens, pos, macros)
                val = rshift(left, right)
            end if
            left = val
        end do
        val = left
    end function

    recursive integer function parse_additive(tokens, ntokens, pos, macros) result(val)
        type(token), intent(in)   :: tokens(:)
        integer, intent(in)         :: ntokens
        integer, intent(inout)      :: pos
        type(macro), intent(in)   :: macros(:)
        !private
        integer :: left, right

        left = parse_multiplicative(tokens, ntokens, pos, macros)
        do while (pos <= ntokens .and. (tokens(pos)%value == '+' .or. tokens(pos)%value == '-'))
            if (verbose) print *, "Parsing ", trim(tokens(pos)%value), " at pos ", pos
            if (tokens(pos)%value == '+') then
                pos = pos + 1
                right = parse_multiplicative(tokens, ntokens, pos, macros)
                val = left + right
            else
                pos = pos + 1
                right = parse_multiplicative(tokens, ntokens, pos, macros)
                val = left - right
            end if
            left = val
        end do
        val = left
    end function
    
    recursive integer function parse_multiplicative(tokens, ntokens, pos, macros) result(val)
        type(token), intent(in)   :: tokens(:)
        integer, intent(in)         :: ntokens
        integer, intent(inout)      :: pos
        type(macro), intent(in)   :: macros(:)
        !private
        integer :: left, right

        left = parse_power(tokens, ntokens, pos, macros)
        do while (pos <= ntokens .and. (tokens(pos)%value == '*' .or. tokens(pos)%value == '/' .or. tokens(pos)%value == '%'))
            if (verbose) print *, "Parsing ", trim(tokens(pos)%value), " at pos ", pos
            if (tokens(pos)%value == '*') then
                pos = pos + 1
                right = parse_power(tokens, ntokens, pos, macros)
                val = left * right
            else if (tokens(pos)%value == '/') then
                pos = pos + 1
                right = parse_power(tokens, ntokens, pos, macros)
                val = left / right
            else
                pos = pos + 1
                right = parse_power(tokens, ntokens, pos, macros)
                val = modulo(left, right)
            end if
            left = val
        end do
        val = left
    end function
    
    recursive integer function parse_power(tokens, ntokens, pos, macros) result(val)
        type(token), intent(in)   :: tokens(:)
        integer, intent(in)         :: ntokens
        integer, intent(inout)      :: pos
        type(macro), intent(in)   :: macros(:)
        !private
        integer :: left, right

        left = parse_unary(tokens, ntokens, pos, macros)
        do while (pos <= ntokens .and. (tokens(pos)%value == '**'))
            if (verbose) print *, "Parsing ", trim(tokens(pos)%value), " at pos ", pos
            pos = pos + 1
            right = parse_unary(tokens, ntokens, pos, macros)
            val = left ** right
            left = val
        end do
        val = left
    end function
    
    recursive integer function parse_unary(tokens, ntokens, pos, macros) result(val)
        type(token), intent(in)   :: tokens(:)
        integer, intent(in)         :: ntokens
        integer, intent(inout)      :: pos
        type(macro), intent(in)   :: macros(:)

        if (pos <= ntokens .and. tokens(pos)%value == '!') then
            if (verbose) print *, "Parsing ! at pos ", pos
            pos = pos + 1
            val = merge(0, 1, parse_unary(tokens, ntokens, pos, macros) /= 0)
        else if (pos <= ntokens .and. tokens(pos)%value == '-') then
            if (verbose) print *, "Parsing - at pos ", pos
            pos = pos + 1
            val = -parse_unary(tokens, ntokens, pos, macros)
        else if (pos <= ntokens .and. tokens(pos)%value == '+') then
            if (verbose) print *, "Parsing + at pos ", pos
            pos = pos + 1
            val = parse_unary(tokens, ntokens, pos, macros)
        else if (pos <= ntokens .and. tokens(pos)%value == '~') then
            if (verbose) print *, "Parsing + at pos ", pos
            pos = pos + 1
            val = not(parse_unary(tokens, ntokens, pos, macros))
        else
            val = parse_atom(tokens, ntokens, pos, macros)
        end if
    end function

    recursive integer function parse_atom(tokens, ntokens, pos, macros) result(val)
        type(token), intent(in)     :: tokens(:)
        integer, intent(in)         :: ntokens
        integer, intent(inout)      :: pos
        type(macro), intent(in)     :: macros(:)
        !private
        integer :: i
        character(:), allocatable :: expanded
        logical :: stitch

        if (pos > ntokens) then
            if (verbose) print *, "Error: Unexpected end of expression at pos ", pos
            val = 0
            return
        end if

        if (verbose) print *, "Parsing primary: ", trim(tokens(pos)%value), " at pos ", pos
        if (tokens(pos)%type == 0) then
            val = strtol(tokens(pos)%value)
            pos = pos + 1
        else if (tokens(pos)%type == 2) then
            if (is_defined(tokens(pos)%value, macros)) then
                expanded = expand_macros(tokens(pos)%value, macros, stitch)
                if (.not. evaluate_expression(expanded, macros, val)) val = 0
                if (verbose) print *, "Expanded ", trim(tokens(pos)%value), " to ", trim(expanded), ", val = ", val
            else
                val = 0
            end if
            pos = pos + 1
        else if (tokens(pos)%value == '(') then
            pos = pos + 1
            val = parse_expression(tokens, ntokens, pos, macros)
            if (pos > ntokens .or. tokens(pos)%value /= ')') then
                if (verbose) print *, "Error: Missing closing parenthesis at pos ", pos
                val = 0
            else
                if (verbose) print *, "Parsed ) at pos ", pos
                pos = pos + 1
            end if
        else if (tokens(pos)%type == 4) then
            expanded = trim(tokens(pos)%value)
            val = merge(1, 0, is_defined(expanded, macros))
            if (verbose) print *, "defined(", trim(expanded), ") = ", val
            pos = pos + 1
        else
            if (verbose) print *, "Error: Invalid token in expression: ", trim(tokens(pos)%value)
            val = 0
            pos = pos + 1
        end if
    end function

    subroutine tokenize(expr, tokens, ntokens)
        character(*), intent(in)                :: expr
        type(token), allocatable, intent(out) :: tokens(:)
        integer, intent(out)                    :: ntokens
        !private
        character(:), allocatable :: temp
        integer :: i, pos, len_expr
        logical :: in_word
        logical, save :: in_comment

        if (allocated(tokens)) deallocate (tokens)
        allocate (tokens(MAX_TOKENS))
        ntokens = 0
        temp = trim(adjustl(expr))//' '
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
                ntokens = ntokens + 1
                if (ntokens > MAX_TOKENS) then
                    if (verbose) print *, "Error: Too many tokens in expression"
                    return
                end if
                in_word = .true.
            end if

            if (temp(i:i) == '(' .or. temp(i:i) == ')') then
                tokens(ntokens)%value = temp(i:i)
                tokens(ntokens)%type = parenthesis
                i = i + 1
                in_word = .false.
            else if (temp(i:i + 1) == '&&' .or. temp(i:i + 1) == '||' .or. temp(i:i + 1) == '==' .or. &
                     temp(i:i + 1) == '!=' .or. temp(i:i + 1) == '<=' .or. temp(i:i + 1) == '>=') then
                tokens(ntokens)%value = temp(i:i + 1)
                tokens(ntokens)%type = operator
                i = i + 2
                in_word = .false.
            else if (temp(i:i) == '!') then
                tokens(ntokens)%value = temp(i:i)
                tokens(ntokens)%type = operator
                i = i + 1
                in_word = .false.
            else if (temp(i:i + 1) == '**') then
                tokens(ntokens)%value = temp(i:i + 1)
                tokens(ntokens)%type = operator
                i = i + 2
                in_word = .false.
            else if (temp(i:i + 1) == '<<' .or. temp(i:i + 1) == '>>') then
                tokens(ntokens)%value = temp(i:i + 1)
                tokens(ntokens)%type = operator
                i = i + 2
                in_word = .false.
            else if (temp(i:i) == '<' .or. temp(i:i) == '>' .or. temp(i:i) == '=' .or. &
                     temp(i:i) == '+' .or. temp(i:i) == '-' .or. temp(i:i) == '*' .or. &
                     temp(i:i) == '/' .or. temp(i:i) == '%') then
                tokens(ntokens)%value = temp(i:i)
                tokens(ntokens)%type = operator
                i = i + 1
                in_word = .false.
            else if (temp(i:i) == '&' .or. temp(i:i) == '|' .or. temp(i:i) == '^' .or. &
                     temp(i:i) == '~') then
                tokens(ntokens)%value = temp(i:i)
                tokens(ntokens)%type = operator
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
                    tokens(ntokens)%value = trim(adjustl(temp(i:pos - 1)))
                    tokens(ntokens)%type = defined
                    i = pos + 1
                else
                    pos = i
                    do while (pos <= len_expr .and. temp(pos:pos) /= ' ')
                        pos = pos + 1
                    end do
                    tokens(ntokens)%value = trim(adjustl(temp(i:pos - 1)))
                    tokens(ntokens)%type = defined
                    i = pos
                end if
                in_word = .false.
            else if (is_typeless(temp(i:), pos)) then
                pos = i + pos
                tokens(ntokens)%value = trim(adjustl(temp(i:pos - 1)))
                tokens(ntokens)%type = number
                i = pos
                in_word = .false.
            else if (is_digit(temp(i:i))) then
                pos = i
                do while (pos <= len_expr .and. is_digit(temp(pos:pos)))
                    pos = pos + 1
                end do
                tokens(ntokens)%value = trim(adjustl(temp(i:pos - 1)))
                tokens(ntokens)%type = number
                i = pos
                in_word = .false.
            else
                pos = i
                do while (pos <= len_expr .and. temp(pos:pos) /= ' ' .and. &
                          temp(pos:pos) /= '(' .and. temp(pos:pos) /= ')')
                    pos = pos + 1
                end do
                tokens(ntokens)%value = trim(temp(i:pos - 1))
                tokens(ntokens)%type = identifier
                i = pos
                in_word = .false.
            end if
        end do

        if (verbose) print *, "Tokens for '", trim(expr), "':"
        do i = 1, ntokens
            if (verbose) print *, "  Token ", i, ": ", trim(tokens(i)%value), " (type ", tokens(i)%type, ")"
        end do
    end subroutine
end module
