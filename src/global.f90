!> @file
!! @defgroup group_global Global
!! Global configuration and shared runtime state for the fpx preprocessor.
!!
!! This module defines the central configuration object used throughout the
!! entire preprocessing session. A single public instance,
!! @link fpx_global::global global @endlink, stores all persistent settings controlling the behavior of
!! the preprocessor.
!!
!! The global configuration provides:
!!
!! - User-defined macro definitions.
!! - Symbols explicitly excluded via `#undef`.
!! - Additional include search directories.
!! - Feature switches controlling optional extensions.
!! - Behavioural settings affecting parsing and expansion.
!! - Runtime flags used by interactive preprocessing sessions.
!!
!! All fpx components access the same global state, avoiding the need to pass
!! configuration objects through every procedure call.
!!
!! The design assumes the traditional single-threaded preprocessing model.
!! If multiple preprocessing jobs are executed concurrently, each instance
!! should maintain its own independent configuration object.
!!
!! @section global_features Supported configuration options
!!
!! The following settings are available:
!!
!! - `macros(:)`
!!   Collection of predefined macros available before preprocessing begins.
!!
!! - `undef(:)`
!!   Symbols protected from future redefinition through `#define`.
!!
!! - `includedir(:)`
!!   Additional directories searched by `#include`.
!!
!! - `expand_macros`
!!   Enables or disables macro expansion globally.
!!
!! - `exclude_comments`
!!   Controls whether comments are preserved in the generated output.
!!
!! - `implicit_continuation`
!!   Enables implicit continuation during macro expansion.
!!
!! - `line_break`
!!   Interprets a double backslash (`\\`) as an explicit line break.
!!
!! - `extra_macros`
!!   Enables non-standard predefined macros such as:
!!   - `__FILE__`
!!   - `__LINE__`
!!   - `__FUNC__`
!!   - `__TIMESTAMP__`
!!
!! - `interactive`
!!   Enables REPL-style interactive preprocessing.
!!
!! - `support_forloop`
!!   Enables support for the non-standard `#for` / `#endfor` directives.
!!
!! - `disable_continuation`
!!   Disables explicit Fortran continuation handling using trailing `&`.
!!
!! - `support_dollar_insert`
!!   Enables `${NAME}` placeholder substitution during macro expansion.
!!
!! @note
!! All settings can be modified at any time before invoking
!! `preprocess(...)`.
!!
!! @section global_examples Examples
!!
!! 1. Add custom include paths:
!! @code{.f90}
!!    use fpx_global
!!
!!    global%includedir = [ &
!!        string('./include'), &
!!        string('../common'), &
!!        string('/usr/local/include/fpx') ]
!!
!!    call preprocess('main.F90')
!! ...
!! @endcode
!!
!! 2. Predefine macros:
!! @code{.f90}
!!    use fpx_global
!!    use fpx_macro
!!
!!    call add(global%macros, macro('DEBUG','1'))
!!    call add(global%macros, macro('MPI_VERSION','4'))
!!
!!    call preprocess('solver.F90')
!! ...
!! @endcode
!!
!! 3. Disable macro expansion:
!! @code{.f90}
!!    global%expand_macros = .false.
!!
!!    call preprocess('input.F90', 'output.F90')
!! ...
!! @endcode
!!
!! 4. Enable fpx extensions:
!! @code{.f90}
!!    global%support_forloop      = .true.
!!    global%support_dollar_insert = .true.
!!    global%extra_macros         = .true.
!!
!!    call preprocess('templates.F90')
!! ...
!! @endcode
!!
!! 5. Start an interactive preprocessing session:
!! @code
!!    global%interactive = .true.
!!
!!    call preprocess(stdin, stdout)
!! ...
!! @endcode
!!
!! @see 
!! <a href="./group__group__macro.html">macro</a>
!! <a href="./group__group__parser.html">parser</a>
!! <a href="./group__group__include.html">include</a>
module fpx_global
    use fpx_constants
    use fpx_string
    use fpx_macro

    implicit none; private

    !> Global preprocessor configuration and shared runtime state.
    !!
    !! This type encapsulates all user-configurable options controlling the
    !! behaviour of the fpx preprocessor.
    !!
    !! A single public instance, @ref global, is provided and used throughout
    !! the library. Applications may modify its components before starting
    !! preprocessing to customize parsing rules, enable extensions, or
    !! predefine symbols.
    !!
    !! @section global_type_examples Examples
    !!
    !! @code{.f90}
    !!    use fpx_global
    !!    use fpx_macro
    !!
    !!    call add(global%macros, macro('__LFORTRAN__','1'))
    !!    global%extra_macros = .true.
    !!    global%support_forloop = .true.
    !! ...
    !! @endcode
    !!
    !! @section global_type_remarks Remarks
    !!
    !! - The settings remain active for the duration of the preprocessing session.
    !! - Components may be modified at any time before calling `preprocess`.
    !! - The global instance is intended for single-threaded use.
    !!
    !! @ingroup group_global
    type, public :: global_settings
        private
        type(macro), allocatable, public    :: macros(:)        !< Predefined macros available before preprocessing begins.
        type(string), allocatable, public   :: undef(:)         !< Symbols protected from future redefinition.
        type(string), allocatable, public   :: includedir(:)    !< Additional directories searched by `#include`.
        logical, public                     :: expand_macros = .true.   !< Enable global macro expansion.
        logical, public                     :: exclude_comments = .false.    !< Preserve comments in the generated output.
        logical, public                     :: implicit_continuation = .false.  !< Enable implicit continuation during macro expansion.
        logical, public                     :: line_break = .false.  !< Treat `\\` as an explicit output line break.
        logical, public                     :: extra_macros = .true.  !< Enable non-standard predefined macros such as `__FILE__`, `__LINE__`, `__FUNC__`, and `__TIMESTAMP__`.
        logical, public                     :: interactive = .false.  !< Enable interactive REPL mode.
        logical, public                     :: support_forloop = .true.  !< Enable support for `#for` and `#endfor`.
        logical, public                     :: disable_continuation = .false.  !< Disable explicit continuation using trailing `&`.
        logical, public                     :: support_dollar_insert = .true.  !< Enable `${NAME}` placeholder substitution.
    end type

    !> Global preprocessor configuration instance.
    !!
    !! This singleton is automatically initialized with sensible default
    !! values and is shared by all fpx modules during preprocessing.
    !!
    !! Applications typically customize this object before invoking
    !! `preprocess(...)`.
    !!
    !! @ingroup group_global
    type(global_settings), public :: global

end module
