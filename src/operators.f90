!> @file
!! @defgroup group_operators Operators
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
!!    parse_expression        ? parse_conditional
!!    parse_conditional       ? parse_or (parse_or '?' parse_expression ':' parse_conditional)
!!    parse_or                ? parse_and ( '||' parse_and )*
!!    parse_and               ? parse_bitwise_or ( '&&' parse_bitwise_or )*
!!    parse_bitwise_or        ? parse_bitwise_xor ( '|' parse_bitwise_xor )*
!!    parse_bitwise_xor       ? parse_bitwise_and ( '^' parse_bitwise_and )*
!!    parse_bitwise_and       ? parse_equality ( '&' parse_equality )*
!!    parse_equality          ? parse_relational ( ('==' | '!=') parse_relational )*
!!    parse_relational        ? parse_shifting ( ('<' | '>' | '<=' | '>=') parse_shifting )*
!!    parse_shifting          ? parse_additive ( ('<<' | '>>') parse_additive )*
!!    parse_additive          ? parse_multiplicative ( ('+' | '-') parse_multiplicative )*
!!    parse_multiplicative    ? parse_power ( ('*' | '/' | '%') parse_power )*
!!    parse_power             ? parse_unary ( '**' parse_unary )*          (right-associative)
!!    parse_unary             ? ('!' | '-' | '+' | '~') parse_unary
!!                             | parse_atom
!!    parse_atom              ? number
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
!!    - @link fpx_operators::evaluate_expression evaluate_expression @endlink: high-level function that tokenizes and evaluates in one
!! call
!!    - @link fpx_operators::parse_expression parse_expression @endlink: low-level entry point for already-tokenized input
!!
!!    This design guarantees correct operator precedence without the need for an explicit
!!    abstract syntax tree or stack-based shunting-yard algorithm, while remaining easy to
!!    read, maintain, and extend.
module fpx_operators
    use fpx_global
    use fpx_string
    use fpx_constants
    use fpx_macro
    use fpx_logging
    use fpx_token

    implicit none; private

    public :: evaluate_expression, &
            parse_expression

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
    !! @ingroup group_operators
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

    !> Parses a sequence of tokens starting at position `pos` as a full expression.
    !! Entry point for the recursive descent parser. Delegates to parse_or().
    !! @param[in] tokens    Array of tokens to parse
    !! @param[in] ntokens   Number of valid tokens in the array
    !! @param[inout] pos    Current parsing position (updated as tokens are consumed)
    !! @param[in] macros    Defined macros for expansion and `defined()` checks
    !! @return Integer value of the parsed expression
    !!
    !! @b Remarks
    !! @ingroup group_operators
    recursive integer function parse_expression(tokens, ntokens, pos, macros) result(val)
        type(token), intent(in)   :: tokens(:)
        integer, intent(in)         :: ntokens
        integer, intent(inout)      :: pos
        type(macro), intent(in)   :: macros(:)

        val = parse_conditional(tokens, ntokens, pos, macros)
    end function

    !> Parses conditional expressions (?:). Right-associative.
    !! @param[in] tokens    Array of tokens to parse
    !! @param[in] ntokens   Number of valid tokens in the array
    !! @param[inout] pos    Current parsing position (updated as tokens are consumed)
    !! @param[in] macros    Defined macros for expansion and `defined()` checks
    !! @return Integer value of the parsed expression
    !!
    !! @b Remarks
    !! @ingroup group_operators
    recursive integer function parse_conditional(tokens, ntokens, pos, macros) result(val)
        type(token), intent(in) :: tokens(:)
        integer, intent(in) :: ntokens
        integer, intent(inout) :: pos
        type(macro), intent(in) :: macros(:)

        integer :: condition, true_val, false_val

        ! First parse condition at higher precedence
        condition = parse_or(tokens, ntokens, pos, macros)

        ! Check for '?'
        if (pos <= ntokens .and. tokens(pos)%value == '?') then

            if (verbose) print *, "Parsing ? at pos ", pos

            pos = pos + 1

            ! Parse true expression (full expression allowed)
            true_val = parse_expression(tokens, ntokens, pos, macros)

            ! Expect ':'
            if (pos > ntokens .or. tokens(pos)%value /= ':') then
                if (verbose) print *, "Error: expected ':' in conditional expression"
                val = 0
                return
            end if

            pos = pos + 1

            ! Parse false expression (right-associative)
            false_val = parse_conditional(tokens, ntokens, pos, macros)

            ! Evaluate condition
            val = merge(true_val, false_val, condition /= 0)

        else

            val = condition

        end if

    end function

    !> Parses logical OR expressions (`||`).
    !! @param[in] tokens    Array of tokens to parse
    !! @param[in] ntokens   Number of valid tokens in the array
    !! @param[inout] pos    Current parsing position (updated as tokens are consumed)
    !! @param[in] macros    Defined macros for expansion and `defined()` checks
    !! @return Integer value of the parsed expression
    !!
    !! @b Remarks
    !! @ingroup group_operators
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
    !! @ingroup group_operators
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
    !! @ingroup group_operators
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
    !! @ingroup group_operators
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
    !! @ingroup group_operators
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
    !! @ingroup group_operators
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
    !! @ingroup group_operators
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
    !! @ingroup group_operators
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
    !! @ingroup group_operators
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
    !! @ingroup group_operators
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
    !! @ingroup group_operators
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
    !! @ingroup group_operators
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
    !! @ingroup group_operators
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
                expanded = expand_macros(tokens(pos)%value, macros, stitch, global%implicit_continuation)
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
end module
