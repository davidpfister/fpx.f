!> @file
!! @defgroup group_diagnostics Diagnostics
!! Processing of #warning and #error preprocessor directives
!! This module implements the core logic for handling messages (either as
!! warning or errors).
!!
!! <h2  class="groupheader">Examples</h2>
!!
!! 1. Break preprocessing on error:
!! @code{.f90}
!!    #ifdef __vax__
!!    #error "VAX not supported"
!!    #endif
!!    ...
!! @endcode
!!
!! 2. Warn version mismatch:
!! @code{.f90}
!!    #if __STDF__ > 1
!!    #warning "The version of the Fortran language is greater than 1"
!!    #endif
!!    ...
!! @endcode
module fpx_diagnostics
    use, intrinsic :: iso_fortran_env, only: stdout => output_unit
    use fpx_logging
    use fpx_macro
    use fpx_global
    use fpx_string
    use fpx_context

    implicit none; private

    public :: handle_error, &
            handle_warning

contains

    !> Process a #error directive. It causes the preprocessor to report a
    !! fatal error that stops the preprocessor. The string forming the rest
    !! of the line following ‘#error' is printed in the standard error.
    !!
    !! @param[in]    ctx     Context source line containing the #define
    !! @param[inout] macros  Current macro table (updated in-place)
    !! @param[in]    token   Usually 'DEFINE' – keyword matched in lowercase
    !!
    !! @b Remarks
    !! @ingroup group_diagnostics
    subroutine handle_error(ctx, macros, token)
        type(context), intent(in)                   :: ctx
        type(macro), allocatable, intent(inout)     :: macros(:)
        character(*), intent(in)                    :: token
        !private
        integer :: pos

        pos = index(lowercase(ctx%content), token) + len(token)
        error stop trim(adjustl(ctx%content(pos + 1:)))
    end subroutine

    !> Process a #warning directive. It causes the preprocessor to report a
    !! warning that does not stop the preprocessor. The string forming the rest
    !! of the line following ‘#warning' is printed in the standard output.
    !! @param[in]    ctx     Context source line containing the #undef
    !! @param[inout] macros  Current macro table (updated in-place)
    !! @param[in]    token   Usually 'UNDEF' – keyword matched in lowercase
    !!
    !! @b Remarks
    !! @ingroup group_diagnostics
    subroutine handle_warning(ctx, macros, token)
        type(context), intent(in)                   :: ctx
        type(macro), allocatable, intent(inout)     :: macros(:)
        character(*), intent(in)                    :: token
        !private
        integer :: pos

        pos = index(lowercase(ctx%content), token) + len(token)
        write(stdout, '(A)') trim(adjustl(ctx%content(pos + 1:)))
    end subroutine
end module
