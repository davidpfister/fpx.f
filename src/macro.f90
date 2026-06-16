!> @file
!! @defgroup group_macro Macro
!! Macro management and expansion core of the fpx Fortran preprocessor
!!
!! This module implements a complete, standards-inspired macro system supporting:
!! - Object-like and function-like macros
!! - Variadic macros (`...` and `__VA_ARGS__`)
!! - C++20/C23-style `__VA_OPT__` handling for optional variadic content
!! - Parameter stringification (`#param`) and token pasting (`##`)
!! - Built-in predefined macros: `__FILE__`, `__FILENAME__`, `__LINE__`, `__DATE__`, `__TIME__`, `__TIMESTAMP__`, `__FUNC__`
!! - Recursive expansion with circular dependency detection via digraph analysis
!! - Dynamic macro table of `macro` objects with efficient addition, lookup, removal
!! - Full support for nested macro calls and proper argument handling
!!
!! The design allows safe, repeated expansion while preventing infinite recursion.
!! All operations are container-agnostic using allocatable dynamic arrays.
!!
!! @par Expansion Model
!! Macros are expanded recursively.
!! Circular dependencies are detected through dependency graph analysis.
!! Macro lookup is currently linear in the number of defined macros.
!!
!! @section macro_examples Examples
!!
!! 1. Define and use simple macros:
!! @code{.f90}
!!    type(macro), allocatable :: macros(:)
!!    call add(macros, macro('PI', '3.1415926535'))
!!    call add(macros, macro('MSG(x)', 'print *, ″Hello ″, x'))
!!    print *, expand_all(context('area = PI * r**2', 10, './circle.F90', 'circle'), macros, stitch, .false., .false., .true.)
!!    !> prints: area = 3.1415926535 * r**2
!! @endcode
!!
!! 2. Variadic macro with stringification and pasting:
!! @code{.f90}
!!    call add(macros, macro('DEBUG_PRINT(...)', 'print *, ″DEBUG[″, __FILE__, ″:″, __LINE__, ″]: ″, __VA_ARGS__'))
!!    print *, expand_all(context('DEBUG_PRINT(″value =″, x)', 42, 'test.F90', 'text'), macros, stitch, .false., .false., .true.)
!!    !> prints: print *, 'DEBUG[', 'test.F90', ':', 42, ']: ', 'value =', x
!! @endcode
!!
!! 3. Token pasting with ##:
!! @code{.f90}
!!    call add(macros, macro('MAKE_VAR(name,num)', 'var_name_##num'))
!!    print *, expand_all(context('real :: MAKE_VAR(temp,42)', 5, 'file.F90', 'file'), macros, stitch, .false., .false.)
!!    !> prints: real :: var_name_42
!! @endcode
module fpx_macro
    use fpx_constants
    use fpx_logging
    use fpx_path
    use fpx_graph
    use fpx_string
    use fpx_date
    use fpx_logging
    use fpx_context

    implicit none; private

    public :: macro, &
            add, &
            get, &
            insert, &
            clear, &
            remove, &
            size_of

    public :: expand_macros, &
            expand_all, &
            is_defined, &
            read_unit, &
            preprocess_line

    !> Derived type representing a single preprocessor macro
    !! Extends @link fpx_string::string string @endlink with macro-specific fields: replacement value, parameters,
    !! variadic flag, and cyclic self-reference detection.
    !! <h2 class="groupheader">Examples</h2>
    !! @code{.f90}
    !!    type(macro), allocatable :: macros(:)
    !!    call add(macros, macro('PI', '3.1415926535'))
    !! @endcode
    !! <h2 class="groupheader">Constructors</h2>
    !! Initializes a new instance of the @ref macro class
    !! <h3>macro(character(*),  character(*))</h3>
    !! @verbatim type(macro) function macro(character(*) name, (optional) character(*) val) @endverbatim
    !!
    !! @param[in] name macro name
    !! @param[in] val  (optional) value of the macro
    !!
    !! @b Examples
    !! @code{.f90}
    !! type(macro) :: m
    !! m = macro('_WIN32')
    !! @endcode
    !! @return The constructed macro object.
    !!
    !! <h2  class="groupheader">Remarks</h2>
    !! @ingroup group_macro
    type, extends(string) :: macro
        character(:), allocatable :: value  !< Value of the macro
        type(string), allocatable :: params(:)  !< List of parameter for function like macros
        logical :: is_variadic  !< Indicate whether the macro is variadic or not.
        logical :: is_cyclic    !< Indicates whether the macro has cyclic dependencies or not.
        logical :: active = .true.
    end type

    !> @brief Constructor interface for macro type
    !!
    !! @b Remarks
    !! @ingroup group_macro
    interface macro
        !! @cond
        module procedure :: macro_new
        !! @endcond
    end interface

    !> Add one or more macros to a dynamic table
    !!
    !! @b Remarks
    !! @ingroup group_macro
    interface add
        module procedure :: add_item
        module procedure :: add_item_from_name
        module procedure :: add_item_from_name_and_value
        module procedure :: add_range
    end interface

    !> Remove all macros from a table
    !!
    !! @b Remarks
    !! @ingroup group_macro
    interface clear
        module procedure  :: clear_item
    end interface

    !> Retrieve a macro by index
    !!
    !! @b Remarks
    !! @ingroup group_macro
    interface get
        module procedure  :: get_item
    end interface

    !> Insert more macro to a dynamic table
    !!
    !! @b Remarks
    !! @ingroup group_macro
    interface insert
        module procedure :: insert_item
    end interface

    !> Remove a macro at given index
    !!
    !! @b Remarks
    !! @ingroup group_macro
    interface remove
        module procedure :: remove_item
    end interface

    !> Return current number of stored macros
    !!
    !! @b Remarks
    !! @ingroup group_macro
    interface size_of
        module procedure  :: size_item
    end interface

    !> Abstract interface for the main preprocessing routine (used for recursion)
    !! Allows handle_include to recursively call the top-level preprocess_unit routine
    !! without creating circular module dependencies.
    !!
    !! @b Remarks
    !! @ingroup group_include
    interface
        subroutine read_unit(iunit, ounit, macros, from_include)
            import macro; implicit none
            integer, intent(in)                     :: iunit
            integer, intent(in)                     :: ounit
            type(macro), allocatable, intent(inout) :: macros(:)
            logical, intent(in)                     :: from_include
        end subroutine
    end interface

    interface
        recursive function preprocess_line(current_line, ounit, filepath, linenum, macros, stch) result(rst)
            import macro; implicit none
            character(*), intent(in)                :: current_line
            integer, intent(in)                     :: ounit
            character(*), intent(inout)             :: filepath
            integer, intent(inout)                  :: linenum
            type(macro), allocatable, intent(inout) :: macros(:)
            logical, intent(out)                    :: stch
            character(:), allocatable               :: rst
        end function
    end interface
contains

    !> Construct a new macro object
    !! @param[in] name Mandatory macro name
    !! @param[in] val  Optional replacement text (default: empty)
    !! @return Initialized macro object
    type(macro) function macro_new(name, val) result(that)
        character(*), intent(in)            :: name
        character(*), intent(in), optional  :: val

        that = trim(name)
        if (present(val)) then
            that%value = val
        else
            that%value = ''
        end if
        allocate(that%params(0))
        that%is_variadic = .false.
        that%is_cyclic = that == that%value
        that%active = .true.
    end function

    !> Fully expand a line including predefined macros (__FILE__, __LINE__, etc.)
    !! First performs normal macro expansion via expand_macros(), then substitutes
    !! standard predefined tokens with current file/line/date information.
    !! @param[in]  ctx  Context
    !! @param[inout]  macros   Current macro table
    !! @param[out] stitch   Set to .true.true. if result ends with '&' (Fortran continuation)
    !! @param[in]  has_extra   Has extra macros (non-standard) like __FILENAME__ and __TIMESTAMP__
    !! @param[in]  implicit_conti If .true., implicit continuation is permitted
    !! @param[in]  dollar_insert If .true., the syntax ${} is supported for macro insertion
    !! @return Expanded line with all macros and predefined tokens replaced
    !!
    !! @b Remarks
    !! @ingroup group_macro
    function expand_all(ctx, macros, stitch, has_extra, implicit_conti, dollar_insert) result(expanded)
        type(context), intent(in)               :: ctx
        type(macro), allocatable, intent(inout) :: macros(:)
        logical, intent(out)                    :: stitch
        logical, intent(in)                     :: has_extra
        logical, intent(in)                     :: implicit_conti
        logical, intent(in)                     :: dollar_insert
        character(:), allocatable :: expanded
        !private
        integer :: pos, start, sep, dot, imacro
        type(datetime) :: date

        if (has_extra) then
            if (.not. is_defined('__FUNC__', macros, imacro)) then
                call add(macros, '__FUNC__', '')
            end if
        end if

        expanded = expand_macros(ctx%content, macros, stitch, implicit_conti, dollar_insert, ctx)

        date = now()

        ! Substitute __FILE__ (relative path to working directory)
        pos = 1
        do while (pos > 0)
            pos = index(expanded, '__FILE__')
            if (pos > 0) then
                start = pos + len('__FILE__')
                expanded = trim(expanded(:pos - 1) // '"' // trim(ctx%path) // '"' // trim(expanded(start:)))
            end if
        end do

        ! Substitute __LINE__
        pos = 1
        do while (pos > 0)
            pos = index(expanded, '__LINE__')
            if (pos > 0) then
                if (pos > 0) then
                    start = pos + len('__LINE__')
                    expanded = trim(expanded(:pos - 1) // tostring(ctx%line) // trim(expanded(start:)))
                end if
            end if
        end do

        ! Substitute __DATE__
        pos = 1
        do while (pos > 0)
            pos = index(expanded, '__DATE__')
            if (pos > 0) then
                if (pos > 0) then
                    start = pos + len('__DATE__')
                    expanded = trim(expanded(:pos - 1) // '"' // date%to_string('MMM-dd-yyyy') // '"' // trim(expanded(start:)))
                end if
            end if
        end do

        ! Substitute __TIME__
        pos = 1
        do while (pos > 0)
            pos = index(expanded, '__TIME__')
            if (pos > 0) then
                if (pos > 0) then
                    start = pos + len('__TIME__')
                    expanded = trim(expanded(:pos - 1) // '"' // date%to_string('HH:mm:ss') // '"' // trim(expanded(start:)))
                end if
            end if
        end do

        if (has_extra) then
            ! Substitute __FILENAME__
            pos = 1; do while (pos > 0)
                pos = index(expanded, '__FILENAME__')
                if (pos > 0) then
                    start = pos + len('__FILENAME__')
                    expanded = trim(expanded(:pos - 1) // '"' // filename(ctx%path, .true.) // '"' // trim(expanded(start:)))
                end if
            end do

            ! Substitute __TIMESTAMP__
            pos = 1; do while (pos > 0)
                pos = index(expanded, '__TIMESTAMP__')
                if (pos > 0) then
                    if (pos > 0) then
                        start = pos + len('__TIMESTAMP__')
                        expanded = trim(expanded(:pos - 1) // '"' // date%to_string('ddd MM yyyy') // ' ' // date%to_string(&
                                'HH:mm:ss'&
                                &) // '"' // trim(expanded(start:)))
                    end if
                end if
            end do
        end if
    end function

    !> Core recursive macro expander (handles function-like, variadic, #, ##)
    !!
    !! Performs actual macro replacement with full support for:
    !! - Function-like macros with argument collection
    !! - Stringification (`#param`)
    !! - Token pasting (`##`)
    !! - Variadic macros and `__VA_ARGS__`, `__VA_OPT__`
    !! - Recursion with cycle detection via digraph
    !! - Proper handling of nested parentheses and quoted strings
    !!
    !! @param[in]  line  Line to be expanded
    !! @param[inout]  macros Current macro table
    !! @param[out] stitch .true. if final line ends with '&'
    !! @param[in]  implicit_conti If .true., implicit continuation is permitted
    !! @param[in]  dollar_insert If .true., ${} macro substitution is supported
    !! @param[in]  ctx  Context
    !! @return Line with user-defined macros expanded (predefined tokens untouched)
    !!
    !! @b Remarks
    !! @ingroup group_macro
    function expand_macros(line, macros, stitch, implicit_conti, dollar_insert, ctx) result(expanded)
        character(*), intent(in)                :: line
        type(macro), allocatable, intent(inout) :: macros(:)
        logical, intent(out)                    :: stitch
        logical, intent(in)                     :: implicit_conti
        logical, intent(in)                     :: dollar_insert
        type(context), intent(in)               :: ctx
        character(:), allocatable               :: expanded
        !private
        integer :: imacro, paren_level
        type(digraph) :: graph

        imacro = 0; paren_level = 0
        graph = digraph(size(macros))
        stitch = .false.

        expanded = expand_macros_internal(line, imacro, macros)

        if (implicit_conti) then
            stitch = (tail(expanded) == '&') .or. paren_level > 0
        else
            stitch = (tail(expanded) == '&') .and. paren_level > 0
        end if
    contains
        !> @private
        recursive function expand_macros_internal(line, imacro, macros) result(expanded)
            character(*), intent(in)                :: line
            integer, intent(in)                     :: imacro
            type(macro), allocatable, intent(inout) :: macros(:)
            character(:), allocatable :: expanded
            !private
            character(:), allocatable :: args_str, temp, va_args
            character(:), allocatable :: token1, token2, prefix, suffix
            type(string) :: arg_values(MAX_PARAMS)
            integer :: c, i, j, k, n, pos, start, arg_start, nargs
            integer :: m_start, m_end, token1_start, token2_stop
            logical :: isopened, found
            character :: quote
            integer, allocatable :: indexes(:)
            logical :: exists, ok, hasfunc

            expanded = line
            if (size(macros) == 0) return
            isopened = .false.; hasfunc = .false.

            do i = 1, size(macros)
                n = len_trim(macros(i)); if (n == 0) cycle
                c = 0
                do while (c < len_trim(expanded))
                    c = c + 1
                    if (expanded(c:c) == '"' .or. expanded(c:c) == "'") then
                        if (.not. isopened) then
                            isopened = .true.
                            quote = expanded(c:c)
                        else
                            if (expanded(c:c) == quote) isopened = .false.
                        end if
                    end if
                    if (isopened) cycle
                    if (c + n - 1 > len_trim(expanded)) exit

                    if (.not. hasfunc) then
                        call update_func_macro(expanded, macros)
                        hasfunc = .true.
                    end if

                    ! Placeholder expansion: ${NAME}
                    if (dollar_insert) then
                        if (expanded(c:c) == '$') then
                            if (c < len_trim(expanded)) then
                                if (expanded(c + 1:c + 1) == '{') then
                                    j = c + 2
                                    do while (j <= len_trim(expanded))
                                        if (expanded(j:j) == '}') exit
                                        j = j + 1
                                    end do

                                    if (j <= len_trim(expanded)) then
                                        token1 = trim(expanded(c + 2:j - 1))
                                        if (is_defined(token1, macros, idx=k)) then
                                            temp = macros(k)%value
                                            if (len(temp) == 0 .and. .not. macros(k)%active) then
                                                c = j
                                            else
                                                expanded = expanded(:c - 1) // temp // expanded(j + 1:)
                                                if (len(temp) /= 0) then
                                                    c = c + len_trim(temp) - 1
                                                end if
                                            end if
                                            cycle
                                        end if
                                    end if
                                end if
                            end if
                        end if
                    end if

                    found = .false.
                    if (expanded(c:c + n - 1) == macros(i)) then
                        found = .true.
                        if (len_trim(expanded(c:)) > n) then
                            found = verify(expanded(c + n:c + n), ' ()[]<>&;.,^~!/*-+\="' // "'") == 0
                        end if
                        if (found .and. c > 1) then
                            found = verify(expanded(c - 1:c - 1), ' ()[]<>&;.,^~!/*-+\="' // "'") == 0
                        end if
                    end if

                    if (found) then
                        pos = c
                        c = c + n - 1
                        m_start = pos
                        start = pos + n
                        ok = allocated(macros(i)%params); if (ok) ok = size(macros(i)%params) > 0
                        if (ok .or. macros(i)%is_variadic) then
                            if (start <= len(expanded)) then
                                if (expanded(start:start) == '(') then
                                    paren_level = 1
                                    arg_start = start + 1
                                    nargs = 0
                                    j = arg_start
                                    do while (j <= len(expanded) .and. paren_level > 0)
                                        if (expanded(j:j) == '(') paren_level = paren_level + 1
                                        if (expanded(j:j) == ')') paren_level = paren_level - 1
                                        if (paren_level == 1 .and. expanded(j:j) == ',' .or. paren_level == 0) then
                                            if (nargs < MAX_PARAMS) then
                                                nargs = nargs + 1
                                                arg_values(nargs) = trim(adjustl(expanded(arg_start:j - 1)))
                                                arg_start = j + 1
                                            end if
                                        end if
                                        j = j + 1
                                    end do
                                    m_end = j - 1
                                    args_str = expanded(start:m_end)
                                    temp = trim(macros(i)%value)

                                    if (macros(i)%is_variadic) then
                                        if (nargs < size(macros(i)%params)) then
                                            call printf(render(diagnostic_report(LEVEL_ERROR, &
                                                    message='Variadic macro issue', &
                                                    label=label_type('Too few arguments for macro ' // macros(i), start, m_end - &
                                                    start), &
                                                    source=trim(ctx%path)), &
                                                    expanded, ctx%line))
                                            cycle
                                        end if
                                        va_args = ''
                                        do j = size(macros(i)%params) + 1, nargs
                                            if (j > size(macros(i)%params) + 1) va_args = va_args // ', '
                                            va_args = va_args // arg_values(j)
                                        end do
                                    else if (nargs /= size(macros(i)%params)) then
                                        call printf(render(diagnostic_report(LEVEL_ERROR, &
                                                message='Function-like macro issue', &
                                                label=label_type('Incorrect number of arguments for macro ' // macros(i), start, &
                                                m_end - start), &
                                                source=trim(ctx%path)), &
                                                expanded, ctx%line))
                                        cycle
                                    end if

                                    ! Substitute regular parameters
                                    argbck :block
                                        integer :: c1, h1
                                        logical :: opened

                                        opened = .false.
                                        jloop: do j = 1, size(macros(i)%params)
                                            c1 = 0
                                            wloop: do while (c1 < len_trim(temp))
                                                c1 = c1 + 1
                                                if (temp(c1:c1) == '"') opened = .not. opened
                                                if (opened) cycle wloop
                                                if (c1 + len_trim(macros(i)%params(j)) - 1 > len(temp)) cycle wloop

                                                if (temp(c1:c1 + len_trim(macros(i)%params(j)) - 1) == trim(macros(i)%params(j))) &
                                                        then
                                                    checkbck:block
                                                        integer :: cend, l

                                                        cend = c1 + len_trim(macros(i)%params(j))
                                                        l = len(temp)
                                                        if (c1 == 1 .and. cend == l + 1) then
                                                            exit checkbck
                                                        else if (c1 > 1 .and. l == cend - 1) then
                                                            if (verify(temp(c1 - 1:c1 - 1), ' #()[]<>&;.,!/*-+\="' // "'") /= 0) &
                                                                    cycle wloop
                                                        else if (c1 <= 1 .and. cend <= l) then
                                                            if (verify(temp(cend:cend), ' #()[]<>&;.,!/*-+\="' // "'") /= 0) cycle &
                                                                    wloop
                                                        else
                                                            if (verify(temp(c1 - 1:c1 - 1), ' #()[]<>&;.,!/*-+\="' // "'") /= 0 &
                                                                    .or. verify(temp(cend:cend), ' #()[]<>$&;.,!/*-+\="' // "'") /=&
                                                                    & 0) cycle wloop
                                                        end if
                                                    end block checkbck
                                                    pos = c1
                                                    c1 = c1 + len_trim(macros(i)%params(j)) - 1
                                                    start = pos + len_trim(macros(i)%params(j))
                                                    if (pos == 2) then
                                                        if (temp(pos - 1:pos - 1) == '#') then
                                                            temp = trim(temp(:pos - 2) // '"' // arg_values(j) // '"' // trim(temp(&
                                                                    start:)))
                                                        else
                                                            temp = trim(temp(:pos - 1) // arg_values(j) // trim(temp(start:)))
                                                        end if
                                                    elseif (pos > 2) then
                                                        h1 = pos - 1
                                                        if (previous(temp, h1) == '#') then
                                                            if (h1 == 1) then
                                                                temp = trim(temp(:h1 - 1) // '"' // arg_values(j) // '"' // trim(&
                                                                        temp(start:)))
                                                            else
                                                                if (temp(h1 - 1:h1 - 1) /= '#') then
                                                                    temp = trim(temp(:h1 - 1) // '"' // arg_values(j) // '"' // &
                                                                            trim(temp(start:)))
                                                                else
                                                                    temp = trim(temp(:pos - 1) // arg_values(j) // trim(temp(start:&
                                                                            )))
                                                                end if
                                                            end if
                                                        else
                                                            temp = trim(temp(:pos - 1) // arg_values(j) // trim(temp(start:)))
                                                        end if
                                                    else
                                                        temp = trim(temp(:pos - 1) // arg_values(j) // trim(temp(start:)))
                                                    end if
                                                end if
                                            end do wloop
                                        end do jloop
                                    end block argbck

                                    ! Handle concatenation (##) first with immediate substitution
                                    block
                                        pos = 1
                                        do while (pos > 0)
                                            pos = index(temp, '##')
                                            if (pos > 0) then
                                                ! Find token1 (before ##)
                                                k = pos - 1
                                                if (k <= 0) then
                                                    call printf(render(diagnostic_report(LEVEL_ERROR, &
                                                            message='Syntax error', &
                                                            label=label_type('No token before ##', pos, 2), &
                                                            source=trim(ctx%path)), &
                                                            temp, ctx%line))
                                                    cycle
                                                end if

                                                token1 = adjustr(temp(:k))
                                                prefix = ''
                                                token1_start = index(token1, ' ')
                                                if (token1_start > 0) then
                                                    prefix = token1(:token1_start)
                                                    token1 = token1(token1_start + 1:)
                                                end if

                                                ! Find token2 (after ##)
                                                k = pos + 2
                                                if (k > len(temp)) then
                                                    call printf(render(diagnostic_report(LEVEL_ERROR, &
                                                            message='Syntax error', &
                                                            label=label_type('No token after ##', pos, 2), &
                                                            source=trim(ctx%path)), &
                                                            temp, ctx%line))
                                                    cycle
                                                end if

                                                suffix = ''
                                                token2 = adjustl(temp(k:))
                                                token2_stop = index(token2, ' ')
                                                if (token2_stop > 0) then
                                                    suffix = token2(token2_stop:)
                                                    token2 = token2(:token2_stop - 1)
                                                end if

                                                ! Concatenate, replacing the full 'token1 ## token2' pattern
                                                if (is_defined(token1, macros, idx=k)) &
                                                        token1 = expand_macros_internal(token1, imacro, macros)
                                                if (is_defined(token2, macros, idx=k)) &
                                                        token2 = expand_macros_internal(token2, imacro, macros)

                                                temp = trim(prefix // trim(token1) // trim(token2) // suffix)
                                            end if
                                        end do
                                    end block

                                    ! Substitute __VA_ARGS__
                                    block
                                        if (macros(i)%is_variadic) then
                                            pos = 1
                                            do while (pos > 0)
                                                pos = index(temp, '__VA_ARGS__')
                                                if (pos > 0) then
                                                    start = pos + len('__VA_ARGS__') - 1
                                                    if (start < len(temp) .and. temp(start:start) == '_' &
                                                            .and. temp(start + 1:start + 1) == ')') then
                                                        temp = trim(temp(:pos - 1) // trim(va_args) // ')')
                                                    else
                                                        temp = trim(temp(:pos - 1) // trim(va_args) // trim(temp(start + 1:)))
                                                    end if

                                                    ! Substitute __VA_OPT__
                                                    pos = index(temp, '__VA_OPT__')
                                                    if (pos > 0) then
                                                        start = pos + index(temp(pos:), ')') - 1
                                                        if (len_trim(va_args) > 0) then
                                                            temp = trim(temp(:pos - 1)) // temp(pos + index(temp(pos:), '('):start &
                                                                    - 1) // trim(temp(start + 1:))
                                                        else
                                                            temp = trim(temp(:pos - 1)) // trim(temp(start + 1:))
                                                        end if
                                                    end if
                                                end if
                                            end do
                                        end if
                                    end block

                                    call graph%add_edge(imacro, i)
                                    if (.not. graph%is_circular(i)) then
                                        temp = expand_macros_internal(temp, i, macros)  ! Only for nested macros
                                    else
                                        call printf(render(diagnostic_report(LEVEL_ERROR, &
                                                message='Failed macro expansion', &
                                                label=label_type('Circular macro detected', index(temp, macros(i)), len(macros(i)))&
                                                , &
                                                source=trim(ctx%path)), &
                                                temp, ctx%line))
                                        cycle
                                    end if
                                    expanded = trim(expanded(:m_start - 1) // trim(temp) // expanded(m_end + 1:))
                                end if
                            end if
                        else
                            temp = trim(macros(i)%value)
                            m_end = start - 1
                            call graph%add_edge(imacro, i)
                            if ((.not. graph%is_circular(i)) .and. (.not. macros(i)%is_cyclic)) then
                                expanded = trim(expanded(:m_start - 1) // trim(temp) // expanded(m_end + 1:))
                                expanded = expand_macros_internal(expanded, imacro, macros)
                            else
                                call printf(render(diagnostic_report(LEVEL_ERROR, &
                                        message='Failed macro expansion', &
                                        label=label_type('Circular macro detected', index(temp, macros(i)), len(macros(i))), &
                                        source=trim(ctx%path)), &
                                        temp, ctx%line))
                                cycle
                            end if
                        end if
                    end if
                end do
            end do
            pos = index(expanded, '&')
            if (index(expanded, '!') > pos .and. pos > 0) expanded = expanded(:pos + 1)
        end function
    end function

    !> Check if a macro with given name exists in table
    !! @param[in] name   Macro name to test
    !! @param[in] macros Current macro table
    !! @param[inout] idx Optional: returns index (1-based) if found
    !! @return .true. if macro is defined
    !!
    !! @b Remarks
    !! @ingroup group_macro
    logical function is_defined(name, macros, idx) result(res)
        character(*), intent(in)            :: name
        type(macro), intent(in)             :: macros(:)
        integer, intent(inout), optional    :: idx
        !private
        integer :: i

        res = .false.
        do i = 1, size(macros)
            if (macros(i) == trim(name)) then
                res = .true.
                if (present(idx)) idx = i
                exit
            end if
        end do
    end function

    !> Generic conversion of polymorphic value to string
    !! Used internally during macro argument stringification and debugging.
    !! Supports integers, reals, logicals, characters, and complex.
    !!
    !! @b Remarks
    !! @ingroup group_macro
    function tostring(any)
        class(*), intent(in) :: any
        !private
        character(:), allocatable   :: tostring
        character(4096)             :: line

        call print_any(any); tostring = trim(line)
    contains
        !> @private
        subroutine print_any(any)
            use, intrinsic :: iso_fortran_env, only: int8, &
                    int16, &
                    int32, &
                    int64, &
                    real32, &
                    real64, &
                    real128
            class(*), intent(in)     :: any

            select type (any)
            type is (integer(kind=int8)); write(line, '(i0)') any
            type is (integer(kind=int16)); write(line, '(i0)') any
            type is (integer(kind=int32)); write(line, '(i0)') any
            type is (integer(kind=int64)); write(line, '(i0)') any
            type is (real(kind=real32)); write(line, '(1pg0)') any
            type is (real(kind=real64)); write(line, '(1pg0)') any
            type is (real(kind=real128)); write(line, '(1pg0)') any
            type is (logical); write(line, '(1l)') any
            type is (character(*)); write(line, '(a)') any
            type is (complex(kind=real32)); write(line, '("(",1pg0,",",1pg0,")")') any
            type is (complex(kind=real64)); write(line, '("(",1pg0,",",1pg0,")")') any
            type is (complex(kind=real128)); write(line, '("(",1pg0,",",1pg0,")")') any
            end select
        end subroutine
    end function

    !> Internal helper: grow dynamic macro array in chunks for efficiency
    !! Adds a new macro to the allocatable array.
    !! Also detects direct self-references (A -> A) and marks both sides as cyclic.
    !!
    !! @b Remarks
    subroutine add_to(array, val)
        type(macro), allocatable, intent(inout) :: array(:)
        type(macro), intent(in)                 :: val(..)
        !private
        type(macro), allocatable :: tmp(:)
        logical, allocatable :: isdef(:)
        integer :: i, j, n

        n = size_of(array)

        select rank (val)
        rank(0)
            allocate(isdef(1), source=.false.)
            do i = 1, n
                if (array(i) == val) then
                    array(i) = val
                    isdef(1) = .true.
                end if
            end do
            if (.not. isdef(1)) then
                allocate(tmp(n + 1))
                if (n > 0) tmp(1:n) = array
                tmp(n + 1) = val
                call move_alloc(tmp, array)
                if (allocated(tmp)) deallocate(tmp)
            end if
        rank(1)
            allocate(isdef(size(val)), source=.false.)
            do concurrent (i = 1:n, j = 1:size(val))
                if (array(i) == val(j)) then
                    array(i) = val(j)
                    isdef(j) = .true.
                end if
            end do
            n = size_of(array); allocate(tmp(n + count(isdef)))
            if (n > 0) tmp(1:n) = array
            tmp(n + 1:) = pack(val, isdef)
            call move_alloc(tmp, array)
            if (allocated(tmp)) deallocate(tmp)
        end select

        do i = 1, size_of(array)
            do j = n + 1, size(array)
                if (i == j) cycle
                if (array(i) == array(j)%value .and. array(i)%value == array(j)) then
                    array(i)%is_cyclic = .true.
                end if
            end do
        end do
    end subroutine

    !> Add a complete macro object to the table
    !!
    !! @b Remarks
    subroutine add_item(this, m)
        type(macro), intent(inout), allocatable :: this(:)
        type(macro), intent(in)                 :: m

        call add_to(this, m)
    end subroutine

    !> Add macro by name only (value = empty)
    !!
    !! @b Remarks
    subroutine add_item_from_name(this, name)
        type(macro), intent(inout), allocatable :: this(:)
        character(*), intent(in)                :: name

        if (.not. allocated(this)) allocate(this(0))
        call add_to(this, macro(name))
    end subroutine

    !> Add macro with name and replacement text
    !!
    !! @b Remarks
    subroutine add_item_from_name_and_value(this, name, value)
        type(macro), intent(inout), allocatable :: this(:)
        character(*), intent(in)                :: name
        character(*), intent(in)                :: value

        if (.not. allocated(this)) allocate(this(0))
        call add_to(this, macro(name, value))
    end subroutine

    !> Add multiple macros at once
    !!
    !! @b Remarks
    subroutine add_range(this, m)
        type(macro), intent(inout), allocatable :: this(:)
        type(macro), intent(in)                 :: m(:)

        if (.not. allocated(this)) allocate(this(0))
        call add_to(this, m)
    end subroutine

    !> Remove all macros from table
    !!
    !! @b Remarks
    subroutine clear_item(this)
        type(macro), intent(inout), allocatable :: this(:)

        if (allocated(this)) deallocate(this)
        allocate(this(0))
    end subroutine

    !> Retrieve macro by 1-based index
    !!
    !! @b Remarks
    function get_item(this, key) result(res)
        type(macro), intent(inout)  :: this(:)
        integer, intent(in)         :: key
        type(macro), allocatable    :: res
        !private
        integer :: n

        n = size(this)
        if (key > 0 .and. key <= n) then
            res = this(key)
        end if
    end function

    !> Insert macro at specific position
    !!
    !! @b Remarks
    subroutine insert_item(this, i, m)
        type(macro), intent(inout), allocatable :: this(:)
        integer, intent(in)                     :: i
        type(macro), intent(in)                 :: m
        !private
        integer :: j, count

        if (.not. allocated(this)) allocate(this(0))
        count = size(this)
        call add_to(this, m)

        do j = count, i + 1, -1
            this(j) = this(j - 1)
        end do
        this(i) = m
    end subroutine

    !> Return number of defined macros
    !!
    !! @b Remarks
    pure integer function size_item(x) result(res)
        class(*), dimension(..), intent(in), optional :: x
        res = 0
        if (present(x)) res = size(x)
    end function

    !> Remove macro at given index
    !!
    !! @b Remarks
    subroutine remove_item(this, i)
        type(macro), intent(inout), allocatable :: this(:)
        integer, intent(in)                     :: i
        !private
        type(macro), allocatable :: tmp(:)
        integer :: k, j, n

        if (.not. allocated(this)) allocate(this(0))
        n = size(this)
        if (allocated(this(i)%params)) deallocate(this(i)%params)
        if (n > 1) then
            this(i:n - 1) = this(i + 1:n)
            allocate(tmp(n - 1))
            tmp = this(:n - 1)
            deallocate(this)
            call move_alloc(tmp, this)

            this(:)%is_cyclic = .false.
            do k = 1, size(this)
                do j = 1, size(this)
                    if (this(k) == this(j)%value .and. this(k)%value == this(j)) then
                        this(i)%is_cyclic = .true.
                        this(j)%is_cyclic = .true.
                    end if
                end do
            end do
        else
            deallocate(this); allocate(this(0))
        end if
    end subroutine

    !> Update the special predefined macro __FUNC__
    !!
    !! Examines the current source line and detects whether it introduces
    !! a Fortran procedure definition (`function` or `subroutine`).
    !! When a procedure declaration is found, the macro `__FUNC__` is
    !! created or updated with the procedure name.
    !!
    !! When an `end function`, `endfunction`, `end subroutine`, or
    !! `endsubroutine` statement is encountered, the macro value is
    !! cleared.
    !!
    !! Detection is token based and therefore supports arbitrary valid
    !! Fortran declaration prefixes such as:
    !! - `recursive function foo()`
    !! - `pure elemental function bar()`
    !! - `type(string) function baz() result(res)`
    !! - `module subroutine solve()`
    !!
    !! @param[in]    line    Current source line after continuation handling
    !! @param[inout] macros  Current macro table (updated in-place)
    !!
    !! @b Remarks
    !! @ingroup group_macro
    subroutine update_func_macro(line, macros)
        character(*), intent(in)                :: line
        type(macro), allocatable, intent(inout) :: macros(:)
        !private
        character(:), allocatable :: txt
        character(:), allocatable :: procname
        logical :: leaving
        integer :: imacro

        if (.not. is_defined('__FUNC__', macros, imacro)) return

        txt = lowercase(adjustl(trim(line)))
        procname = extract_proc_name(txt, leaving)

        if (len_trim(procname) > 0) then
            macros(imacro)%value = procname
            return
        end if

        ! Leaving a procedure
        if (starts_with(txt, 'end function') .or. &
                starts_with(txt, 'endfunction') .or. &
                starts_with(txt, 'end subroutine') .or. &
                starts_with(txt, 'endsubroutine')) then

            if (.not. is_defined('__FUNC__', macros, imacro)) then
                call add(macros, '__FUNC__', '')
            else
                macros(imacro)%value = ''
            end if
        end if
    end subroutine

    !> Extract the procedure name from a Fortran procedure declaration
    !!
    !! Searches a source line for a standalone `function` or `subroutine`
    !! token and returns the identifier immediately following it.
    !!
    !! The parser is intentionally independent of declaration prefixes,
    !! allowing valid declarations such as:
    !! @code{.f90}
    !!    function foo()
    !!    recursive function foo()
    !!    pure elemental function foo()
    !!    type(string) function foo() result(res)
    !!    module subroutine solve()
    !! @endcode
    !!
    !! End statements (`end function`, `endfunction`,
    !! `end subroutine`, `endsubroutine`) are ignored and return
    !! an unallocated result.
    !!
    !! @param[in] txt Source line to analyze
    !! @return    Extracted procedure name, unallocated if no procedure
    !!            declaration is found
    !!
    !! @b Remarks
    !! @ingroup group_macro
    function extract_proc_name(txt, leaving) result(name)
        character(*), intent(in)    :: txt
        logical, intent(out)        :: leaving
        character(:), allocatable   :: name
        !private
        integer :: pos, istart, iend
        character(:), allocatable :: tmp

        name = ''
        tmp = lowercase(adjustl(trim(txt)))

        ! Ignore END FUNCTION / END SUBROUTINE
        if (index(tmp, 'end function') > 0) then
            leaving = .true.
            return
        elseif (index(tmp, 'endfunction') > 0) then
            leaving = .true.
            return
        elseif (index(tmp, 'end subroutine') > 0) then
            leaving = .true.
            return
        elseif (index(tmp, 'endsubroutine') > 0) then
            leaving = .true.
            return
        end if

        ! Search FUNCTION token
        pos = find_token(tmp, 'function')

        if (pos > 0) then
            istart = pos + len('function')
        else
            pos = find_token(tmp, 'subroutine')
            if (pos == 0) return
            istart = pos + len('subroutine')
        end if

        ! Skip whitespace
        do while (istart <= len(tmp))
            if (tmp(istart:istart) /= ' ') exit
            istart = istart + 1
        end do

        if (istart > len(tmp)) return

        iend = istart

        do while (iend <= len(tmp))
            select case (tmp(iend:iend))
            case ('a':'z', 'A':'Z', '0':'9', '_')
                iend = iend + 1
            case default
                exit
            end select
        end do

        name = tmp(istart:iend - 1)
    contains
        !> Locate a standalone token within a source line
        !! Searches for a token delimited by non-identifier characters.
        !! The token must not appear as part of a larger identifier.
        !!
        !! Examples:
        !! @code{.f90}
        !!    function foo()      ! match "function"
        !!    subroutine bar()    ! match "subroutine"
        !!    myfunction()        ! no match
        !!    subroutine_name     ! no match
        !! @endcode
        !!
        !! @param[in] line  Source line to search
        !! @param[in] token Token to locate
        !! @return Position of the first valid token occurrence,
        !!         or zero if not found
        !!
        !! @b Remarks
        !! @ingroup group_macro
        integer function find_token(line, token) result(pos)
            character(*), intent(in) :: line
            character(*), intent(in) :: token
            !private
            integer :: i, ltok, lline
            logical :: left_ok, right_ok

            pos = 0
            lline = len_trim(line); ltok = len_trim(token)

            if (ltok == 0 .or. lline < ltok) return

            do i = 1, lline - ltok + 1
                if (lowercase(line(i:i + ltok - 1)) /= lowercase(token)) cycle

                ! Check left boundary
                if (i == 1) then
                    left_ok = .true.
                else
                    left_ok = .not. is_ident(line(i - 1:i - 1))
                end if

                ! Check right boundary
                if (i + ltok - 1 == lline) then
                    right_ok = .true.
                else
                    right_ok = .not. is_ident(line(i + ltok:i + ltok))
                end if

                if (left_ok .and. right_ok) then
                    pos = i
                    return
                end if
            end do
        end function

        !> Determine whether a character is a valid identifier character
        !!
        !! Returns `.true.` for characters that may appear in a Fortran
        !! identifier:
        !! - letters (`A-Z`, `a-z`)
        !! - digits (`0-9`)
        !! - underscore (`_`)
        !!
        !! Used internally by token matching routines to verify identifier
        !! boundaries.
        !!
        !! @param[in] ch Character to test
        !! @return `.true.` if the character is a valid identifier character
        !!
        !! @b Remarks
        !! @ingroup group_macro
        logical function is_ident(ch)
            character(1), intent(in) :: ch

            select case (ch)
            case ('a':'z', 'A':'Z', '0':'9', '_')
                is_ident = .true.
            case default
                is_ident = .false.
            end select
        end function
    end function
end module
