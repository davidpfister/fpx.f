!> @file
!! @defgroup group_define Define
!! Processing of #define and #undef preprocessor directives
!! This module implements the core logic for handling macro definition and removal
!! during preprocessing in the fpx Fortran preprocessor. It supports:
!! - Object-like macros: `#define NAME value`
!! - Function-like macros: `#define NAME(arg1, arg2, ...) replacement`
!! - Variadic macros using `...` and automatic detection
!! - Proper parameter parsing with whitespace handling
!! - Macro redefinition (overwrites existing definition)
!! - Safe `#undef` that removes a previously defined macro
!! - Integration with global undef list (`global%undef`) to block redefinition
!! - Comprehensive verbose logging of all definition actions
!!
!! The routines are designed to be robust against malformed input and provide
!! clear diagnostics when `verbose = .true.`.
!! <h2  class="groupheader">Examples</h2>
!!
!! 1. Define simple object-like macros:
!! @code{.f90}
!!    #define PI 3.141592653589793
!!    #define DEBUG 1
!!    #define MAX_SIZE 1024
!!    ...
!! @endcode
!!
!! 2. Define function-like and variadic macros:
!! @code{.f90}
!!    #define SQR(x) ((x)*(x))
!!    #define LOG_MSG(level, ...)  print *, '[LOG:', level, ']', __VA_ARGS__
!!    #define CONCAT(a,b) a ## _ ## b
!!    ...
!! @endcode
!!
!! 3. Undefine a macro:
!! @code{.f90}
!!    #undef DEBUG
!!    !> Subsequent #ifdef DEBUG will be false
!! @endcode
!!
!! 4. Using from a driver program:
!! @code{.f90}
!!    use fpx_global
!!    use fpx_logging, only: verbose
!!
!!    verbose = .true.
!!    call preprocess('input.F90')   ! Will show all macro definitions/undefs
!!    ...
!! @endcode
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

    !> Process a #define directive and register or update a macro
    !! Parses the line after `#define`, distinguishes between object-like and
    !! function-like forms, handles variadic `...`, extracts parameters correctly,
    !! and stores the macro in the active macro table. Existing macros are
    !! overwritten. Respects `global%undef` list – macros listed there are ignored.
    !!
    !! @param[in]    ctx     Context source line containing the #define
    !! @param[inout] macros  Current macro table (updated in-place)
    !! @param[in]    token   Usually 'DEFINE' – keyword matched in lowercase
    !!
    !! @b Remarks
    !! @ingroup group_define
    subroutine handle_define(ctx, macros, token)
        type(context), intent(in)                   :: ctx
        type(macro), allocatable, intent(inout)     :: macros(:)
        character(*), intent(in)                    :: token
        !private
        character(:), allocatable :: val, name, temp
        integer :: pos, paren_start, paren_end, i, npar, imacro

        pos = index(lowercase(ctx%content), token) + len(token)
        temp = trim(adjustl(ctx%content(pos + 1:)))

        paren_start = index(temp, '(')
        pos = index(temp, ' ')
        if (pos > 0 .and. pos < paren_start) paren_start = 0

        if (paren_start > 0) then
            name = trim(temp(:paren_start - 1))

            if (global%undef .contains. name) return
            paren_end = index(temp, ')')
            if (paren_end == 0) then
                call printf(render(diagnostic_report(LEVEL_ERROR, &
                        message = 'Synthax error', &
                        label = label_type('Missing closing parenthesis in macro definition', len_trim(ctx%content) + 1, 1), &
                        source = ctx%path), &
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
                        message = 'Reserved macro name', &
                        label = label_type('"defined" cannot be used as a macro name', paren_start + 1, len(name)), &
                        source = ctx%path), &
                        trim(ctx%content), ctx%line))
            end if

            if (.not. is_defined(name, macros, imacro)) then
                call add(macros, name, val)
                imacro = sizeof(macros)
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
                imacro = sizeof(macros)
            else
                macros(imacro) = macro(name, val)
            end if
        end if
    end subroutine

    !> Process a #undef directive and remove a macro from the table
    !! Finds the named macro in the current table and removes it.
    !! Issues a warning if the macro was not previously defined.
    !! @param[in]    ctx     Context source line containing the #undef
    !! @param[inout] macros  Current macro table (updated in-place)
    !! @param[in]    token   Usually 'UNDEF' – keyword matched in lowercase
    !!
    !! @b Remarks
    !! @ingroup group_define
    subroutine handle_undef(ctx, macros, token)
        type(context), intent(in)                   :: ctx
        type(macro), allocatable, intent(inout)     :: macros(:)
        character(*), intent(in)                    :: token
        !private
        type(macro), allocatable :: temp_macros(:)
        character(:), allocatable :: name
        integer :: i, n, pos

        n = sizeof(macros)
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
                        message = 'Unknown macro', &
                        label = label_type(name // ' not found', pos, len(name)), &
                        source = ctx%path), &
                        trim(ctx%content)))
        end if
    end subroutine
end module
