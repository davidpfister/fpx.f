!> @file
!! @defgroup group_token Token
!! Module implementing a full C-preprocessor-style constant expression evaluator using a top-down recursive descent parser.
!! The module provides the ability to evaluate integer constant expressions of the kind used in
!! classical preprocessor. This includes support for:
!! - All C-style arithmetic, bitwise, logical, relational, and conditional operators
!! - Operator precedence and associativity
!! - Macro identifier substitution and the special `defined(identifier)` operator
!! - Integer literals in decimal, octal (`0...`), hexadecimal (`0x...`), and binary (`0b...`) bases
!! - Parenthesized sub-expressions and proper handling of unary operators
!!
!! The implementation consists of two major phases:
!!
!! 1. Tokenization
!!    The input string is scanned and converted into a sequence of @link fpx_token::token token @endlink objects.
!!    The tokenizer recognizes multi-character operators ('&&', '||', '==', '!=', '<=', '>=', '<<', '>>', '**'),
!!    the `defined` operator (with or without parentheses), numbers in all supported bases,
!!    identifiers, and parentheses. Whitespace is ignored except as a token separator.
!!
!! 2. Parsing and evaluation via top-down recursive descent
!!    A classic predictive (LL(1)) recursive descent parser is used, where each non-terminal
!!    in the grammar is implemented as a separate parsing function with the exact precedence level.
!!    The grammar is directly derived from the C standard operator precedence table:
!!
!!    parse_expression        → parse_or
!!    parse_or                → parse_and ( '||' parse_and )*
!!    parse_and               → parse_bitwise_or ( '&&' parse_bitwise_or )*
!!    parse_bitwise_or        → parse_bitwise_xor ( '|' parse_bitwise_xor )*
!!    parse_bitwise_xor       → parse_bitwise_and ( '^' parse_bitwise_and )*
!!    parse_bitwise_and       → parse_equality ( '&' parse_equality )*
!!    parse_equality          → parse_relational ( ('==' | '!=') parse_relational )*
!!    parse_relational        → parse_shifting ( ('<' | '>' | '<=' | '>=') parse_shifting )*
!!    parse_shifting          → parse_additive ( ('<<' | '>>') parse_additive )*
!!    parse_additive          → parse_multiplicative ( ('+' | '-') parse_multiplicative )*
!!    parse_multiplicative    → parse_power ( ('*' | '/' | '%') parse_power )*
!!    parse_power             → parse_unary ( '**' parse_unary )*          (right-associative)
!!    parse_unary             → ('!' | '-' | '+' | '~') parse_unary
!!                             | parse_atom
!!    parse_atom              → number
!!                             | identifier                              (macro expansion)
!!                             | 'defined' ( identifier ) | 'defined' identifier
!!                             | '(' parse_expression ')'
!!
!!    Each parsing function consumes tokens from the global position `pos` and returns the
!!    integer value of the sub-expression it recognizes. Because the grammar is factored by
!!    precedence, left-associativity is achieved naturally via left-recursive loops,
!!    while right-associativity for the power operator (`**`) is handled by calling
!!    `parse_unary` on the right-hand side first.
!!
!!    Macro expansion occurs lazily inside `parse_atom` when an identifier token is encountered:
!!    - If the identifier is defined, its replacement text is recursively evaluated.
!!    - The special `defined` operator yields 1 or 0 depending on whether the identifier exists.
!!
!!    The parser is fully re-entrant and has no global state except the `verbose` flag from
!!    the logging module (used only for debugging).
!!
!!    Public interface:
!!    - @link fpx_token::evaluate_expression evaluate_expression @endlink: high-level function that tokenizes and evaluates in one
!! call
!!    - @link fpx_token::parse_expression parse_expression @endlink: low-level entry point for already-tokenized input
!!
!!    This design guarantees correct operator precedence without the need for an explicit
!!    abstract syntax tree or stack-based shunting-yard algorithm, while remaining easy to
!!    read, maintain, and extend.
module fpx_token
    use fpx_string
    use fpx_constants
    use fpx_macro
    use fpx_logging

    implicit none; private

    public :: evaluate_expression, &
            parse_expression

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
    !! @ingroup group_token
    interface strtol
        !! @cond
        module procedure :: strtol_default
        module procedure :: strtol_with_base
        !! @endcond
    end interface

contains

    !> Evaluates a preprocessor-style expression with macro substitution.
    !! Tokenizes the input expression, expands macros where appropriate,
    !! parses it according to operator precedence, and computes the integer result.
    !! Returns .true. if evaluation succeeded and the result is non-zero.
    !!
    !! @param[in] expr   Expression string to evaluate
    !! @param[in] macros Array of defined macros for substitution and `defined()` checks
    !! @param[out] val   Optional integer result of the evaluation
    !! @return .true. if the expression evaluated successfully to non-zero, .false. otherwise
    !!
    !! @b Remarks
    !! @ingroup group_token
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

    !> Parses a sequence of tokens starting at position `pos` as a full expression.
    !! Entry point for the recursive descent parser. Delegates to parse_or().
    !! @param[in] tokens    Array of tokens to parse
    !! @param[in] ntokens   Number of valid tokens in the array
    !! @param[inout] pos    Current parsing position (updated as tokens are consumed)
    !! @param[in] macros    Defined macros for expansion and `defined()` checks
    !! @return Integer value of the parsed expression
    !!
    !! @b Remarks
    !! @ingroup group_token
    recursive integer function parse_expression(tokens, ntokens, pos, macros) result(val)
        type(token), intent(in)   :: tokens(:)
        integer, intent(in)         :: ntokens
        integer, intent(inout)      :: pos
        type(macro), intent(in)   :: macros(:)

        val = parse_or(tokens, ntokens, pos, macros)
    end function

    !> Parses logical OR expressions (`||`).
    !! @param[in] tokens    Array of tokens to parse
    !! @param[in] ntokens   Number of valid tokens in the array
    !! @param[inout] pos    Current parsing position (updated as tokens are consumed)
    !! @param[in] macros    Defined macros for expansion and `defined()` checks
    !! @return Integer value of the parsed expression
    !!
    !! @b Remarks
    !! @ingroup group_token
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

    !> Parses logical AND expressions (`&&`).
    !! @param[in] tokens    Array of tokens to parse
    !! @param[in] ntokens   Number of valid tokens in the array
    !! @param[inout] pos    Current parsing position (updated as tokens are consumed)
    !! @param[in] macros    Defined macros for expansion and `defined()` checks
    !! @return Integer value of the parsed expression
    !!
    !! @b Remarks
    !! @ingroup group_token
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

    !> Parses bitwise OR expressions (`|`).
    !! @param[in] tokens    Array of tokens to parse
    !! @param[in] ntokens   Number of valid tokens in the array
    !! @param[inout] pos    Current parsing position (updated as tokens are consumed)
    !! @param[in] macros    Defined macros for expansion and `defined()` checks
    !! @return Integer value of the parsed expression
    !!
    !! @b Remarks
    !! @ingroup group_token
    recursive integer function parse_bitwise_or(tokens, ntokens, pos, macros) result(val)
        type(token), intent(in)   :: tokens(:)
        integer, intent(in)       :: ntokens
        integer, intent(inout)    :: pos
        type(macro), intent(in)   :: macros(:)
        !private
        integer :: left

        left = parse_bitwise_xor(tokens, ntokens, pos, macros)
        do while (pos <= ntokens .and. tokens(pos)%value == '|')
            if (verbose) print *, "Parsing | at pos ", pos
            pos = pos + 1
            val = parse_bitwise_xor(tokens, ntokens, pos, macros)
            left = ior(left, val)
        end do
        val = left
    end function

    !> Parses bitwise XOR expressions (`^`).
    !! @param[in] tokens    Array of tokens to parse
    !! @param[in] ntokens   Number of valid tokens in the array
    !! @param[inout] pos    Current parsing position (updated as tokens are consumed)
    !! @param[in] macros    Defined macros for expansion and `defined()` checks
    !! @return Integer value of the parsed expression
    !!
    !! @b Remarks
    !! @ingroup group_token
    recursive integer function parse_bitwise_xor(tokens, ntokens, pos, macros) result(val)
        type(token), intent(in)   :: tokens(:)
        integer, intent(in)       :: ntokens
        integer, intent(inout)    :: pos
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

    !> Parses bitwise AND expressions (`&`).
    !! @param[in] tokens    Array of tokens to parse
    !! @param[in] ntokens   Number of valid tokens in the array
    !! @param[inout] pos    Current parsing position (updated as tokens are consumed)
    !! @param[in] macros    Defined macros for expansion and `defined()` checks
    !! @return Integer value of the parsed expression
    !!
    !! @b Remarks
    !! @ingroup group_token
    recursive integer function parse_bitwise_and(tokens, ntokens, pos, macros) result(val)
        type(token), intent(in)   :: tokens(:)
        integer, intent(in)       :: ntokens
        integer, intent(inout)    :: pos
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

    !> Parses equality/inequality expressions (`==`, `!=`).
    !! @param[in] tokens    Array of tokens to parse
    !! @param[in] ntokens   Number of valid tokens in the array
    !! @param[inout] pos    Current parsing position (updated as tokens are consumed)
    !! @param[in] macros    Defined macros for expansion and `defined()` checks
    !! @return Integer value of the parsed expression
    !!
    !! @b Remarks
    !! @ingroup group_token
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

    !> Parses relational expressions (`<`, `>`, `<=`, `>=`).
    !! @param[in] tokens    Array of tokens to parse
    !! @param[in] ntokens   Number of valid tokens in the array
    !! @param[inout] pos    Current parsing position (updated as tokens are consumed)
    !! @param[in] macros    Defined macros for expansion and `defined()` checks
    !! @return Integer value of the parsed expression
    !!
    !! @b Remarks
    !! @ingroup group_token
    recursive integer function parse_relational(tokens, ntokens, pos, macros) result(val)
        type(token), intent(in)   :: tokens(:)
        integer, intent(in)       :: ntokens
        integer, intent(inout)    :: pos
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

    !> Parses shift expressions (`<<`, `>>`).
    !! @param[in] tokens    Array of tokens to parse
    !! @param[in] ntokens   Number of valid tokens in the array
    !! @param[inout] pos    Current parsing position (updated as tokens are consumed)
    !! @param[in] macros    Defined macros for expansion and `defined()` checks
    !! @return Integer value of the parsed expression
    !!
    !! @b Remarks
    !! @ingroup group_token
    recursive integer function parse_shifting(tokens, ntokens, pos, macros) result(val)
        type(token), intent(in)   :: tokens(:)
        integer, intent(in)       :: ntokens
        integer, intent(inout)    :: pos
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

    !> Parses additive expressions (`+`, `-`).
    !! @param[in] tokens    Array of tokens to parse
    !! @param[in] ntokens   Number of valid tokens in the array
    !! @param[inout] pos    Current parsing position (updated as tokens are consumed)
    !! @param[in] macros    Defined macros for expansion and `defined()` checks
    !! @return Integer value of the parsed expression
    !!
    !! @b Remarks
    !! @ingroup group_token
    recursive integer function parse_additive(tokens, ntokens, pos, macros) result(val)
        type(token), intent(in)   :: tokens(:)
        integer, intent(in)       :: ntokens
        integer, intent(inout)    :: pos
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

    !> Parses multiplicative expressions (`*`, `/`, `%`).
    !! @param[in] tokens    Array of tokens to parse
    !! @param[in] ntokens   Number of valid tokens in the array
    !! @param[inout] pos    Current parsing position (updated as tokens are consumed)
    !! @param[in] macros    Defined macros for expansion and `defined()` checks
    !! @return Integer value of the parsed expression
    !!
    !! @b Remarks
    !! @ingroup group_token
    recursive integer function parse_multiplicative(tokens, ntokens, pos, macros) result(val)
        type(token), intent(in)   :: tokens(:)
        integer, intent(in)       :: ntokens
        integer, intent(inout)    :: pos
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

    !> Parses exponentiation (`**`). Right-associative.
    !! @param[in] tokens    Array of tokens to parse
    !! @param[in] ntokens   Number of valid tokens in the array
    !! @param[inout] pos    Current parsing position (updated as tokens are consumed)
    !! @param[in] macros    Defined macros for expansion and `defined()` checks
    !! @return Integer value of the parsed expression
    !!
    !! @b Remarks
    !! @ingroup group_token
    recursive integer function parse_power(tokens, ntokens, pos, macros) result(val)
        type(token), intent(in)   :: tokens(:)
        integer, intent(in)       :: ntokens
        integer, intent(inout)    :: pos
        type(macro), intent(in)   :: macros(:)
        !private
        integer :: left, right

        left = parse_unary(tokens, ntokens, pos, macros)
        do while (pos <= ntokens .and. (tokens(pos)%value == '**'))
            if (verbose) print *, "Parsing ", trim(tokens(pos)%value), " at pos ", pos
            pos = pos + 1
            right = parse_unary(tokens, ntokens, pos, macros)
            val = left**right
            left = val
        end do
        val = left
    end function

    !> Parses unary operators (`!`, `-`, `+`, `~`).
    !! @param[in] tokens    Array of tokens to parse
    !! @param[in] ntokens   Number of valid tokens in the array
    !! @param[inout] pos    Current parsing position (updated as tokens are consumed)
    !! @param[in] macros    Defined macros for expansion and `defined()` checks
    !! @return Integer value of the parsed expression
    !!
    !! @b Remarks
    !! @ingroup group_token
    recursive integer function parse_unary(tokens, ntokens, pos, macros) result(val)
        type(token), intent(in)   :: tokens(:)
        integer, intent(in)       :: ntokens
        integer, intent(inout)    :: pos
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

    !> Parses primary expressions: numbers, identifiers, `defined(...)`, parentheses.
    !! @param[in] tokens    Array of tokens to parse
    !! @param[in] ntokens   Number of valid tokens in the array
    !! @param[inout] pos    Current parsing position (updated as tokens are consumed)
    !! @param[in] macros    Defined macros for expansion and `defined()` checks
    !! @return Integer value of the parsed expression
    !!
    !! @b Remarks
    !! @ingroup group_token
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
