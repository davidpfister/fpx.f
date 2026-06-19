!> @file
!! @defgroup group_define Define
!! Macro definition and removal directives for the fpx preprocessor.
!!
!! This module implements the `#define` and `#undef` directives used to create,
!! update, and remove preprocessor macros during source preprocessing.
!!
!! Supported macro forms include:
!!
!! - Object-like macros:
!!   `#define NAME value`
!!
!! - Function-like macros:
!!   `#define NAME(arg1,arg2,...) replacement`
!!
!! - Variadic macros:
!!   `#define LOG(level, ...) ...`
!!
!! - Empty definitions:
!!   `#define FEATURE`
!!
!! - Macro redefinition:
!!   Existing definitions are replaced by the most recent one.
!!
!! The parser correctly identifies matching parentheses in function-like macro
!! signatures, allowing nested parentheses inside parameter lists. Whitespace
!! surrounding parameters is ignored, and variadic arguments are detected
!! automatically through the `...` notation.
!!
!! The module also implements `#undef`, allowing previously defined symbols
!! to be removed from the active macro table. Symbols listed in
!! `global%undef` are protected from redefinition and silently ignored.
!!
!! All syntax errors are reported through the diagnostic framework, providing
!! source locations and explanatory messages.
!!
!! @note
!! Macro definitions are local to the current preprocessing context unless
!! explicitly propagated by the caller.
!!
!! @section define_examples Examples
!!
!! 1. Object-like macros:
!! @code{.f90}
!!    #define PI        3.141592653589793
!!    #define DEBUG     1
!!    #define VERSION   "1.2.0"
!! ...
!! @endcode
!!
!! 2. Empty definitions:
!! @code{.f90}
!!    #define USE_MPI
!!
!!    #ifdef USE_MPI
!!       !...
!!    #endif
!! ...
!! @endcode
!!
!! 3. Function-like macros:
!! @code{.f90}
!!    #define SQR(x)        ((x)*(x))
!!    #define MIN(a,b)      ((a)<(b)?(a):(b))
!!    #define CONCAT(a,b)   a ## b
!! ...
!! @endcode
!!
!! 4. Variadic macros:
!! @code{.f90}
!!    #define LOG(level, ...) &
!!        print *, "[", level, "]", __VA_ARGS__
!! @endcode
!!
!! 5. Removing a definition:
!! @code{.f90}
!!    #undef DEBUG
!!
!!    #ifdef DEBUG
!!       ! This block is skipped
!!    #endif
!! ...
!! @endcode
!!
!! 6. Redefinition:
!! @code{.f90}
!!    #define SIZE 128
!!    #define SIZE 256
!!
!!    integer :: buf(SIZE)   ! expands to 256
!! ...
!! @endcode
!!
!! 7. Reserved names:
!! @code{.f90}
!!    #define defined(x) 1
!! ...
!! @endcode
!!
!! produces a diagnostic because `defined` is reserved for conditional
!! expressions.
!!
!! @see 
!! <a href="./group__group__macro.html">macro</a> @n
!! <a href="./group__group__global.html">global</a> @n
!! <a href="./group__group__context.html">context</a> 
module fpx_define
    use fpx_constants
    use fpx_logging
    use fpx_macro
    use fpx_string
    use fpx_global
    use fpx_context

    implicit none; private

    public :: handle_define, &
            handle_undef

contains

    !> Process a `#define` directive.
    !!
    !! Parses the directive contained in the supplied context and updates the
    !! active macro table accordingly.
    !!
    !! The routine automatically distinguishes between:
    !!
    !! - object-like macros,
    !! - function-like macros,
    !! - variadic macros using `...`,
    !! - empty definitions.
    !!
    !! Function-like signatures are parsed using matching-parenthesis tracking,
    !! ensuring that the closing parenthesis corresponding to the opening `(`
    !! is located correctly even in the presence of nested parentheses.
    !!
    !! Existing definitions are overwritten. Symbols listed in
    !! `global%undef` are ignored. Attempts to define the reserved identifier
    !! `defined` generate an error diagnostic.
    !!
    !! @param[in]    ctx
    !!   Source context containing the complete `#define` directive.
    !! @param[inout] macros
    !!   Active macro table updated in place.
    !! @param[in]    token
    !!   Directive keyword, typically `"define"`.
    !!
    !! @ingroup group_define
    subroutine handle_define(ctx, macros, token)
        type(context), intent(in)                   :: ctx
        type(macro), allocatable, intent(inout)     :: macros(:)
        character(*), intent(in)                    :: token
        !private
        character(:), allocatable :: val, name, temp
        integer :: pos, paren_start, paren_end, i, npar, imacro, level

        pos = index(lowercase(ctx%content), token) + len(token)
        temp = trim(adjustl(ctx%content(pos + 1:)))

        paren_start = index(temp, '(')
        pos = index(temp, ' ')
        if (pos > 0 .and. pos < paren_start) paren_start = 0

        if (paren_start > 0) then
            name = trim(temp(:paren_start - 1))

            if (global%undef .contains. name) return
            paren_end = 0; level = 0
            do i = paren_start, len_trim(temp)
                select case (temp(i:i))
                case ('(')
                    level = level + 1
                case (')')
                    level = level - 1
                    if (level == 0) then
                        paren_end = i
                        exit
                    end if
                end select
            end do
            if (paren_end == 0) then
                call printf(render(diagnostic_report(LEVEL_ERROR, &
                        message='Syntax error', &
                        label=label_type('Missing closing parenthesis in macro definition', len_trim(ctx%content) + 1, 1), &
                        source=ctx%path), &
                        trim(ctx%content), ctx%line))
                return
            end if
            val = trim(adjustl(temp(paren_end + 1:)))
            temp = temp(paren_start + 1:paren_end - 1)
            npar = 0
            pos = 1
            do while (pos <= len_trim(temp))
                if (temp(pos:pos) == ',') then
                    npar = npar + 1
                end if
                pos = pos + 1
            end do
            if (len_trim(temp) > 0) npar = npar + 1

            if (.not. allocated(macros)) allocate(macros(0))

            if (name == 'defined') then
                call printf(render(diagnostic_report(LEVEL_ERROR, &
                        message='Reserved macro name', &
                        label=label_type('"defined" cannot be used as a macro name', paren_start + 1, len(name)), &
                        source=ctx%path), &
                        trim(ctx%content), ctx%line))
            end if

            if (.not. is_defined(name, macros, imacro)) then
                call add(macros, name, val)
                imacro = size_of(macros)
            else
                macros(imacro) = macro(name, val)
            end if

            if (index(temp, '...') > 0) then
                macros(imacro)%is_variadic = .true.
                npar = npar - 1
                if (allocated(macros(imacro)%params)) deallocate(macros(imacro)%params)
                allocate(macros(imacro)%params(npar))
                pos = 1
                i = 1
                do while (pos <= len_trim(temp) .and. i <= npar)
                    do while (pos <= len_trim(temp) .and. temp(pos:pos) == ' ')
                        pos = pos + 1
                    end do
                    if (pos > len_trim(temp)) exit
                    paren_start = pos
                    do while (pos <= len_trim(temp) .and. temp(pos:pos) /= ',')
                        pos = pos + 1
                    end do
                    macros(imacro)%params(i) = temp(paren_start:pos - 1)
                    i = i + 1
                    pos = pos + 1
                end do
            else
                macros(imacro)%is_variadic = .false.
                if (allocated(macros(imacro)%params)) deallocate(macros(imacro)%params)
                allocate(macros(imacro)%params(npar))
                pos = 1
                i = 1
                do while (pos <= len_trim(temp) .and. i <= npar)
                    do while (pos <= len_trim(temp) .and. temp(pos:pos) == ' ')
                        pos = pos + 1
                    end do
                    if (pos > len_trim(temp)) exit
                    paren_start = pos
                    do while (pos <= len_trim(temp) .and. temp(pos:pos) /= ',' .and. temp(pos:pos) /= ' ')
                        pos = pos + 1
                        if (pos > len_trim(temp)) exit
                    end do
                    macros(imacro)%params(i) = temp(paren_start:pos - 1)
                    i = i + 1
                    if (pos <= len_trim(temp)) then
                        if (temp(pos:pos) == ',') pos = pos + 1
                    end if
                end do
            end if
        else
            pos = index(temp, ' ')
            if (pos > 0) then
                name = trim(temp(:pos - 1))
                val = trim(adjustl(temp(pos + 1:)))
            else
                name = trim(temp)
                val = ''
            end if

            if (global%undef .contains. name) return
            if (.not. allocated(macros)) allocate(macros(0))
            if (.not. is_defined(name, macros, imacro)) then
                call add(macros, name, val)
                imacro = size_of(macros)
            else
                macros(imacro) = macro(name, val)
            end if
        end if
    end subroutine

    !> Process a `#undef` directive.
    !!
    !! Removes the specified macro from the active macro table.
    !! If the requested symbol is not currently defined, a warning
    !! diagnostic is emitted.
    !!
    !! @param[in]    ctx
    !!   Source context containing the complete `#undef` directive.
    !! @param[inout] macros
    !!   Active macro table updated in place.
    !! @param[in]    token
    !!   Directive keyword, typically `"undef"`.
    !!
    !! @ingroup group_define
    subroutine handle_undef(ctx, macros, token)
        type(context), intent(in)                   :: ctx
        type(macro), allocatable, intent(inout)     :: macros(:)
        character(*), intent(in)                    :: token
        !private
        character(:), allocatable :: name
        integer :: i, n, pos

        n = size_of(macros)
        pos = index(lowercase(ctx%content), token) + len(token)
        name = trim(adjustl(ctx%content(pos:)))
        do i = 1, n
            if (macros(i) == name) then
                call remove(macros, i)
                exit
            end if
        end do

        if (i > n) then
            call printf(render(diagnostic_report(LEVEL_WARNING, &
                    message='Unknown macro', &
                    label=label_type(name // ' not found', pos, len(name)), &
                    source=ctx%path), &
                    trim(ctx%content)))
        end if
    end subroutine
end module
