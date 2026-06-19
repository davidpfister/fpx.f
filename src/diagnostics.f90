!> @file
!! @defgroup group_diagnostics Diagnostics
!! Diagnostic directives for the fpx preprocessor.
!!
!! This module implements the handling of the preprocessor directives
!! `#error` and `#warning`, allowing source files to emit user-defined
!! diagnostics during preprocessing.
!!
!! These directives are commonly used to enforce configuration requirements,
!! reject unsupported platforms, report deprecated features, or notify users
!! about assumptions made during compilation.
!!
!! Supported directives:
!!
!! - `#error`
!!   Emits a fatal diagnostic and immediately terminates preprocessing.
!!
!! - `#warning`
!!   Emits a non-fatal diagnostic message while allowing preprocessing to
!!   continue normally.
!!
!! The diagnostic text consists of the remainder of the directive line
!! following the keyword itself.
!!
!! @note
!! The routines implemented in this module do not perform macro expansion on
!! the diagnostic message. Any expansion must have been completed before the
!! directive handler is invoked.
!!
!! @section diagnostics_examples Examples
!!
!! 1. Reject unsupported platforms:
!! @code{.f90}
!!    #ifdef __VAX__
!!    #error "VAX systems are not supported."
!!    #endif
!! ...
!! @endcode
!!
!! 2. Enforce configuration requirements:
!! @code{.f90}
!!    #ifndef MPI_VERSION
!!    #error "MPI support must be enabled."
!!    #endif
!! ...
!! @endcode
!!
!! 3. Warn about deprecated functionality:
!! @code{.f90}
!!    #ifdef USE_LEGACY_SOLVER
!!    #warning "USE_LEGACY_SOLVER is deprecated and will be removed."
!!    #endif
!! ...
!! @endcode
!!
!! 4. Notify users of unusual configurations:
!! @code{.f90}
!!    #if PRECISION > 64
!!    #warning "Using extended precision may affect performance."
!!    #endif
!! ...
!! @endcode
!!
!! 5. Emit custom informational messages:
!! @code{.f90}
!!    #warning "Building experimental version."
!! ...
!! @endcode
!!
!! @see
!! <a href="./group__group__logging.html">logging</a> @n
!! <a href="./group__group__context.html">context</a>
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

    !> Process a `#error` directive.
    !!
    !! Extracts the message following the directive keyword and immediately
    !! terminates preprocessing using an `error stop` statement.
    !!
    !! This directive is intended for unrecoverable situations such as
    !! unsupported targets, invalid configurations, or missing prerequisites.
    !!
    !! @param[in]    ctx
    !!   Source context containing the complete `#error` directive.
    !! @param[inout] macros
    !!   Active macro table. Present for interface consistency and not modified.
    !! @param[in]    token
    !!   Directive keyword, typically `"error"`.
    !!
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

    !> Process a `#warning` directive.
    !!
    !! Extracts the message following the directive keyword and writes it to
    !! the standard output stream without interrupting preprocessing.
    !!
    !! This directive is intended for non-fatal conditions such as deprecated
    !! features, unusual build settings, or informational notices.
    !!
    !! @param[in]    ctx
    !!   Source context containing the complete `#warning` directive.
    !! @param[inout] macros
    !!   Active macro table. Present for interface consistency and not modified.
    !! @param[in]    token
    !!   Directive keyword, typically `"warning"`.
    !!
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
