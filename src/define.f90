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
    !! @param[in]    line    Full source line containing the #define
    !! @param[inout] macros  Current macro table (updated in-place)
    !! @param[in]    token   Usually 'DEFINE' – keyword matched in lowercase
    !!
    !! @b Remarks
    !! @ingroup group_define
    subroutine handle_define(line, macros, token)
        character(*), intent(in)                    :: line
        type(macro), allocatable, intent(inout)     :: macros(:)
        character(*), intent(in)                    :: token
        !private
        character(:), allocatable :: val, name, temp
        integer :: pos, paren_start, paren_end, i, npar, imacro

        pos = index(lowercase(line), token) + len(token)
        temp = trim(adjustl(line(pos + 1:)))

        paren_start = index(temp, '(')
        pos = index(temp, ' ')
        if (pos > 0 .and. pos < paren_start) paren_start = 0

        if (paren_start > 0) then
            name = trim(temp(:paren_start - 1))

            if (global%undef .contains. name) return
            paren_end = index(temp, ')')
            if (paren_end == 0) then
                if (verbose) print *, "Error: Unclosed parenthesis in macro definition: ", trim(line)
                return
            end if
            val = trim(adjustl(temp(paren_end + 1:)))
            if (verbose) print *, "Raw value before allocation: ", val, ", length = ", len(val)

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
                if (verbose) print *, '"defined" cannot be used a a macro name'
                return
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
                    if (verbose) print *, "Param ", i, ": '", macros(imacro)%params(i), &
                            "', length = ", len_trim(macros(imacro)%params(i))
                    i = i + 1
                    pos = pos + 1
                end do
                if (verbose) print *, "Defined variadic macro: ", trim(name), &
                        "(", (macros(imacro)%params(i) // ", ", i = 1, npar), "...) = ", trim(val)
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
                    if (verbose) print *, "Param ", i, ": '", trim(macros(imacro)%params(i)), &
                            "', length = ", len_trim(macros(imacro)%params(i))
                    i = i + 1
                    if (pos <= len_trim(temp)) then
                        if (temp(pos:pos) == ',') pos = pos + 1
                    end if
                end do
                if (verbose) print *, "Defined macro: ", trim(name), "(", (macros(imacro)%params(i) // ", ", i = 1, npar - 1), &
                        macros(imacro)%params(npar), ") = ", trim(val)
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

            if (verbose) print *, "Defined macro: ", trim(name), " = ", trim(val)
        end if
    end subroutine

    !> Process a #undef directive and remove a macro from the table
    !! Finds the named macro in the current table and removes it.
    !! Issues a warning if the macro was not previously defined.
    !! @param[in]    line    Full source line containing the #undef
    !! @param[inout] macros  Current macro table (updated in-place)
    !! @param[in]    token   Usually 'UNDEF' – keyword matched in lowercase
    !!
    !! @b Remarks
    !! @ingroup group_define
    subroutine handle_undef(line, macros, token)
        character(*), intent(in)                    :: line
        type(macro), allocatable, intent(inout)     :: macros(:)
        character(*), intent(in)                    :: token
        !private
        type(macro), allocatable :: temp_macros(:)
        character(:), allocatable :: name
        integer :: i, n, pos

        n = sizeof(macros)
        pos = index(lowercase(line), token) + len(token)
        name = trim(adjustl(line(pos:)))
        do i = 1, n
            if (macros(i) == name) then
                if (verbose) print *, "Undefining macro: ", name
                call remove(macros, i)
                exit
            end if
        end do

        if (i > n) then
            if (verbose) print *, "Warning: Macro ", name, " not found for #undef"
        end if
    end subroutine
end module
