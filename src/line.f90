!> @file
!! @defgroup group_line Line
!! Standard `#line` directive support for the fpx Fortran preprocessor
!!
!! This module implements full support for the ISO C99/C11 `#line` directive,
!! which is also widely used in Fortran preprocessors.
!!
!! The `#line` directive allows changing the logical line number and/or source
!! filename reported by the preprocessor. It is particularly useful for:
!! - Code generators
!! - Literate programming tools
!! - Macro-heavy generated code
!! - Accurate `__LINE__` and `__FILE__` expansion in included/generated files
!!
!! Supported forms (standard compliant):
!! - `#line <number>`
!! - `#line <number> "<filename>"`
!!
!! When a `#line` directive is encountered, the current context (line number and
!! filename) is updated immediately. This affects all subsequent diagnostics,
!! `__LINE__`, `__FILE__`, and `__FILENAME__` macros.
!!
!! <h2  class="groupheader">Examples</h2>
!!
!! 1. Basic line number reset:
!! @code{.f90}
!!    #line 100
!!    print *, "This line will be reported as line 100"
!! @endcode
!!
!! 2. Changing both line number and filename (common in generated code):
!! @code{.f90}
!!    #line 42 "generated_code.f90"
!!    integer :: x = 1   ! This will appear as line 42 in generated_code.f90
!! @endcode
!!
!! @par Remarks
!! - The line number in `#line N` refers to the **next** line after the directive.
!! - Filename must be quoted if provided.
!! - Invalid line numbers or malformed directives emit a warning but are ignored.
!! - Updates the shared `context` object used by the logging and macro systems.   
module fpx_line
    use fpx_path
    use fpx_logging
    use fpx_context
    
    implicit none; private
    
contains

    !> Handle the standard #line directive
    !! Supports two standard forms:
    !!   #line <number>
    !!   #line <number> "<filename>"
    !!
    !! This updates the current line number (`ctx%line`) and optionally the current
    !! filename (`ctx%path`) for subsequent diagnostics, `__LINE__`, and `__FILE__`
    !! expansions.
    !!
    !! Fully compliant with ISO C99 / C11 �6.10.4 and common Fortran preprocessor behavior.
    !!
    !! @param[inout] ctx     Context source line containing the #line directive
    !! @param[in]    token   Usually 'DEFINE' � keyword matched in lowercase
    !!
    !! @b Remarks
    !! @ingroup group_line
    subroutine handle_line(ctx, token)
        type(context), intent(inout)    :: ctx
        character(*), intent(in)        :: token
        !private
        character(:), allocatable :: temp, num_str, fname
        integer :: pos, iostat, new_line
        logical :: has_filename

        ! Skip #line keyword
        pos = index(lowercase(ctx%content), token) + len(token)
        temp = trim(adjustl(ctx%content(pos:)))

        if (len_trim(temp) == 0) then
            call printf(render(diagnostic_report(LEVEL_WARNING, &
                        message='Synthax error', &
                        label=label_type('#line directive with no arguments', index(token, lowercase(ctx%content)) + len(token) + 1, 1), &
                        source=ctx%path), &
                        trim(ctx%content), ctx%line))
            return
        end if

        ! Extract line number
        pos = index(temp, ' ')
        if (pos > 0) then
            num_str = temp(:pos-1)
            fname = trim(adjustl(temp(pos:)))
            has_filename = .true.
        else
            num_str = trim(temp)
            has_filename = .false.
        end if

        ! Parse line number
        read(num_str, *, iostat=iostat) new_line
        if (iostat /= 0 .or. new_line < 1) then
            call printf(render(diagnostic_report(LEVEL_WARNING, &
                        message='Synthax error', &
                        label=label_type('Invalid line number in #line directive', index(token, lowercase(ctx%content)) + len(token) + 1, len(num_str)), &
                        source=ctx%path), &
                        trim(ctx%content), ctx%line))
        end if

        ! Update current line number (subtract 1 because the next line will be +1)
        ctx%line = new_line - 1

        ! Update filename if provided (strip quotes)
        if (has_filename) then
            if (fname(1:1) == '"' .and. len(fname) > 1) then
                fname = fname(2:index(fname(2:), '"'))
            end if
            if (len_trim(fname) > 0) then
                ctx%path = trim(fname)
            end if
        end if
    end subroutine
end module