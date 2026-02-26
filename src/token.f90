!> @file
!! @defgroup group_token Token
!! @brief Token classification and representation for expression parsing in fpx
!!
!! This module provides the lightweight but robust token infrastructure used by the
!! fpx preprocessor when evaluating constant expressions in `#if` / `#elif` directives.
!!
!! It defines:
!! - A clean enumeration of token kinds (`tokens_enum`)
!! - A simple `token` derived type that carries both the lexical value and its semantic category
!!
!! These types are used internally by `evaluate_expression()` (from `fpx_token`) to parse
!! and compute `#if DEBUG > 1 && defined(USE_MPI)`-style conditions.
!!
!! @par Key design goals
!! - Minimal memory footprint
!! - Clear separation between lexical scanning and semantic interpretation
!! - Easy extensibility for future operators or functions
!!
!! @par Examples
!!
!! 1. Manual token creation (mostly for testing/debugging):
!! @code{.f90}
!!    use fpx_token
!!    
!!    type(token) :: t1, t2, t3
!!    
!!    t1 = token('42',   number)         ! numeric literal
!!    t2 = token('DEBUG', identifier)    ! macro name
!!    t3 = token('>',    operator)       ! comparison operator
!!    
!!    print *, 'Token: ', t1%value, ' type=', t1%type   ! → 42 type=0
!! @endcode
!!
!! 2. Typical internal usage during `#if` evaluation:
!! @code{.f90}
!!    ! (inside evaluate_expression)
!!    tokens = tokenize('defined(USE_MPI) && MPI_VERSION >= 3')
!!    ! tokens(1) → value=vdefined'  type=identifier
!!    ! tokens(2) → value='('        type=parenthesis
!!    ! tokens(3) → value='USE_MPI'  type=identifier
!!    ! ...
!! @endcode
!!
!! @par Token kinds overview
!! | Enumerator   | Value | Meaning                                      |
!! |--------------|-------|----------------------------------------------|
!! | `unknown`    | -1    | Invalid / unrecognized token                 |
!! | `number`     |  0    | Integer or floating-point literal            |
!! | `operator`   |  1    | ?:, +, -, *, /, ==, !=, &&, ||, !, >, <, etc.|
!! | `identifier` |  2    | Macro name or function name (e.g. `defined`) |
!! | `parenthesis`|  3    | `(` or `)`                                   |
!! | `defined`    |  4    | Special keyword `defined` (treated specially)|
!!
module fpx_token
    use fpx_constants, only: MAX_TOKENS
    use fpx_string
    use fpx_logging
    
    implicit none; private

    public :: tokenize, &
            strtol, &
            tokens_enum, &
            unknown, &
            number, &
            operator, &
            identifier, &
            parenthesis, &
            defined

    !> @brief Token kinds used in expression parsing.
    !! Enumeration defining the possible types of tokens recognized by the tokenizer.
    !! @ingroup group_token
    enum, bind(c)
        enumerator :: unknown = -1
        enumerator :: number = 0
        enumerator :: operator = 1
        enumerator :: identifier = 2
        enumerator :: parenthesis = 3
        enumerator :: defined = 4
    end enum

    !> @brief Kind parameter for token type enumeration. Values are (`unknown`, `number`, `operator`, `identifier`, `parenthesis`,
    !! `defined`)
    !! @ingroup group_token
    integer, parameter :: tokens_enum = kind(unknown)

    !> Represents a single token in a parsed expression.
    !! Holds the string value of the token and its classified type.
    !! <h2 class="groupheader">Constructors</h2>
    !! Initializes a new instance of the @link fpx_token::token token @endlink class
    !! <h3>token(character(:), integer)</h3>
    !! @verbatim type(token) function token(character(:) value, integer type) @endverbatim
    !!
    !! @param[in] value
    !! @param[in] type
    !!
    !! @b Examples
    !! @code{.f90}
    !! a = token('9', number)
    !! @endcode
    !!
    !! <h2 class="groupheader">Remarks</h2>
    !! @ingroup group_token
    type, public :: token
        character(:), allocatable   :: value  !< Token value
        integer(tokens_enum)        :: type  !< Token type, from the enum @ref tokens_enum.
    end type
    
    !> Converts a string to integer.
    !! <h2 class="groupheader">Methods</h2>
    !!
    !! @code{.f90}strtol(character(*) str, (optional) logical success)@endcode
    !!
    !! @param[in]  str      String to convert
    !! @param[out] success  Optional flag indicating successful conversion
    !! @return Converted integer value
    !!
    !! @code{.f90}strtol(character(*) str, integer base, (optional) logical success)@endcode
    !!
    !! Converts a string to integer with explicit base handling.
    !! Supports base 2, 8, 10, 16 and prefixes `0x`, `0b`.
    !! @param[in]    str      String to convert
    !! @param[inout] base     0 = auto-detect, otherwise forces given base
    !! @param[out]   success  Optional flag indicating successful conversion
    !! @return Converted integer value
    !!
    !! <h2 class="groupheader"> Examples </h2>
    !! The following demonstrate a call to the `strtol` interface.
    !! @code{.f90}
    !!  integer :: i
    !!  logical :: success
    !!
    !!  i = strtol('    123', 0, success = res)
    !!  ! i = 123
    !! @endcode
    !!
    !! <h2 class="groupheader"> Remarks </h2>
    !! @ingroup group_operators
    interface strtol
        !! @cond
        module procedure :: strtol_default
        module procedure :: strtol_with_base
        !! @endcond
    end interface

contains
    
    !> Tokenizes a preprocessor expression into an array of token structures.
    !! Handles whitespace, multi-character operators (`&&`, `||`, `==`, etc.),
    !! the `defined` operator (with or without parentheses), numbers in various bases,
    !! identifiers, and parentheses.
    !! @param[in]  expr     Expression string to tokenize
    !! @param[out] tokens   Allocated array receiving the tokens
    !! @param[out] ntokens  Number of tokens produced
    !!
    !! @b Remarks
    !! @ingroup group_token
    subroutine tokenize(expr, tokens, ntokens)
        character(*), intent(in)                :: expr
        type(token), allocatable, intent(out) :: tokens(:)
        integer, intent(out)                    :: ntokens
        !private
        character(:), allocatable :: temp
        integer :: i, pos, len_expr
        logical :: in_word
        logical, save :: in_comment

        if (allocated(tokens)) deallocate(tokens)
        allocate(tokens(MAX_TOKENS))
        ntokens = 0
        temp = trim(adjustl(expr)) // ' '
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
                        temp(i:i) == '/' .or. temp(i:i) == '%' .or. &
                        temp(i:i) == '?' .or. temp(i:i) == ':') then
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

    !> Tests whether a single character is a decimal digit ('0'-'9').
    !! @param[in] ch Character to test
    !! @return .true. if ch is a digit
    !!
    !! @b Remarks
    !! @ingroup group_token
    logical elemental function is_digit(ch) result(res)
        character(*), intent(in) :: ch

        res = verify(ch, '0123456789') == 0
    end function

    !> Detects whether a string starts a typeless constant (hex, octal, binary).
    !! Used to avoid treating them as identifiers during tokenization.
    !! @param[in]  str Input string starting at current position
    !! @param[out] pos Length of the typeless constant (0 if not typeless)
    !! @return .true. if the prefix is a valid typeless constant in non-base-10
    !!
    !! @b Remarks
    !! @ingroup group_token
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
        if (pos > 0) i = strtol(str(:pos - 1), base, success=res)
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
        work_str = adjustl(str)  ! Remove leading spaces
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
            digit = -1  ! Invalid digit marker

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
end module
