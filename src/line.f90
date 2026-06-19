!> @file
!! @defgroup group_line Line
!! Standard-compliant handling of the `#line` directive.
!!
!! This module implements support for the ISO C preprocessor `#line`
!! directive, allowing the logical source location used by the preprocessor
!! to be modified during preprocessing.
!!
!! The directive affects the information stored in the active
!! @link fpx_context::context context @endlink object and therefore influences:
!!
!! - Diagnostic messages and source locations,
!! - Expansions of predefined macros such as `__LINE__`,
!! - Expansions of `__FILE__` and `__FILENAME__`,
!! - The apparent origin of generated or transformed source code.
!!
!! This functionality is particularly useful for:
!!
!! - Source-to-source translators,
!! - Code generators,
!! - Literate programming systems,
!! - Template engines,
!! - Tools that emit Fortran intended to preserve original source locations.
!!
!! The following standard forms are supported:
!!
!! - `#line <number>`
!! - `#line <number> "<filename>"`
!!
!! When encountered, the directive immediately updates the logical
!! source position used for all subsequent processing.
!!
!! @note
!! The specified line number refers to the line immediately following
!! the directive itself, matching the behaviour of the ISO C preprocessor.
!!
!! @note
!! Malformed directives generate warnings and are ignored.
!!
!! @section line_examples Examples
!!
!! 1. Reset the logical line number:
!! @code{.f90}
!!    #line 100
!!    print *, "Reported as line 100"
!! ...
!! @endcode
!!
!! 2. Change both line number and filename:
!! @code{.f90}
!!    #line 42 "generated.f90"
!!    integer :: x
!!
!!    ! Diagnostics now refer to generated.f90:42
!! ...
!! @endcode
!!
!! 3. Improve diagnostics in generated code:
!! @code{.f90}
!!    #line 215 "input_template.f90"
!!    call generated_procedure()
!! ...
!! @endcode
module fpx_line
    use fpx_path
    use fpx_logging
    use fpx_context

    implicit none; private

    public :: handle_line

contains

    !> Process a `#line` directive.
    !!
    !! Parses the directive arguments, validates the requested logical
    !! line number, and updates the active @ref context object.
    !!
    !! Supported forms are:
    !!
    !! @code{.txt}
    !! #line <number>
    !! #line <number> "<filename>"
    !! @endcode
    !!
    !! The specified line number becomes the number associated with the
    !! source line immediately following the directive.
    !!
    !! If a filename is supplied, subsequent diagnostics and predefined
    !! file-related macros use the new filename.
    !!
    !! Invalid directives produce warnings and leave the current context
    !! unchanged.
    !!
    !! @param[inout] ctx
    !!    Current source context. Its logical line number and optional
    !!    filename are updated in place.
    !! @param[in] token
    !!    Directive keyword used to identify the `#line` directive,
    !!    typically `"line"`.
    !!
    !! @ingroup group_line
    subroutine handle_line(ctx, token)
        type(context), intent(inout)    :: ctx
        character(*), intent(in)        :: token
        !private
        character(:), allocatable :: temp, num_str, fname
        integer :: pos, iostat, new_line, closing
        logical :: has_filename

        ! Skip #line keyword
        pos = index(lowercase(ctx%content), token) + len(token)
        temp = trim(adjustl(ctx%content(pos:)))

        if (len_trim(temp) == 0) then
            call printf(render(diagnostic_report(LEVEL_WARNING, &
                    message='Syntax error', &
                    label=label_type('#line directive with no arguments', index(token, lowercase(ctx%content)) + len(token) + 1, 1)&
                    , &
                    source=ctx%path), &
                    trim(ctx%content), ctx%line))
            return
        end if

        ! Extract line number
        pos = index(temp, ' ')
        if (pos > 0) then
            num_str = temp(:pos - 1)
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
                    message='Syntax error', &
                    label=label_type('Invalid line number in #line directive', index(token, lowercase(ctx%content)) + len(token) + &
                    1, len(num_str)), &
                    source=ctx%path), &
                    trim(ctx%content), ctx%line))
            return
        end if

        ! Update current line number (subtract 1 because the next line will be +1)
        ctx%line = new_line - 1

        ! Update filename if provided (strip quotes)
        if (has_filename) then
            if (fname(1:1) == '"' .and. len(fname) > 1) then
                closing = index(fname(2:), '"')
                if (closing == 0) then
                    call printf(render(diagnostic_report(LEVEL_ERROR, &
                        message='Malformed #line directive', &
                        label=label_type('Missing closing quotation mark', &
                        index(ctx%content,'"'),1), &
                        source=trim(ctx%path)), &
                        ctx%content, ctx%line))
                end if
                fname = fname(2:closing)
            end if
            if (len_trim(fname) > 0) then
                ctx%path = trim(fname)
            end if
        end if
    end subroutine
end module
