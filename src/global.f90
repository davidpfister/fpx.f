!> @file
!! @defgroup group_global Global
!! Central global configuration and shared state for the fpx Fortran preprocessor
!! This module defines a single global instance `global` of type `global_settings`
!! that holds all persistent, user-configurable state used across the entire preprocessing session:
!!
!! - `macros(:)`         : Dynamic table of all defined macros (object-like and function-like)
!! - `undef(:)`          : List of symbols explicitly undefined via `#undef`
!! - `includedir(:)`     : User-specified include search directories for `#include <...>`
!! - `expand_macros`     : Master switch to enable/disable macro expansion (default: .true.)
!! - `exclude_comments`  : When .true., strip C-style /*...*/ and Fortran ! comments from output
!!
!! The design uses a single public variable `global` so that all fpx modules can access
!! and modify the same configuration without passing arguments everywhere.
!! This is safe in single-threaded use (typical for preprocessing) and allows easy
!! customization from driver programs or interactive sessions.
!! <h2  class="groupheader">Examples</h2>
!!
!! 1. Add custom include paths before preprocessing:
!! @code{.f90}
!!
!!    global%includedir = [ './include', '/usr/local/include/fortran', '../common' ]
!!    call preprocess('main.F90', 'main.f90')
!!    !#include <file.h> will search these directories
!! @endcode
!!
!! 2. Predefine common macros (e.g. for conditional compilation):
!! @code{.f90}
!!
!!    call add(global%macros, macro('DEBUG', '1'))
!!    call add(global%macros, macro('MPI_VERSION', '3'))
!!    call add(global%macros, macro('USE_OPENMP', '1'))
!!
!!    call preprocess('src/app.F90')
!!    !> Code can now use #ifdef DEBUG, #if MPI_VERSION >= 3, etc.
!! @endcode
!!
!! 3. Disable macro expansion temporarily (pass-through mode):
!! @code{.f90}
!!    global%expand_macros = .false.   ! Only handle #include and conditionals
!!    call preprocess('raw_source.F90', 'clean.F90')
!!    ...
!! @endcode
!!
!! 4. Strip all comments from final output:
!! @code{.f90}
!!    global%exclude_comments = .true.
!!    call preprocess('messy.F90', 'clean_no_comments.f90')
!!    ...
!! @endcode
module fpx_global
    use fpx_constants
    use fpx_string
    use fpx_logging
    use fpx_macro

    implicit none; private

    !> Global preprocessor configuration and shared runtime state
    !! All components of fpx read from and write to this single instance.
    !! Users can safely modify its public components at any time.
    !! <h2  class="groupheader">Examples</h2>
    !! @code{.f90}
    !!    use fpx_global
    !!
    !!    call add(global%macros, macro('__LFORTRAN__','1'))
    !!    call add(global%macros, macro('__VERSION__'))
    !!    call add(global%macros, macro('__LFORTRAN_MAJOR__'))
    !!    call add(global%macros, macro('__LFORTRAN_MINOR__'))
    !!    call add(global%macros, macro('__LFORTRAN_PATCHLEVEL__'))
    !! @endcode
    !! <h2  class="groupheader">Remarks</h2>
    !! @par
    !! The global settings are accessed through the global variable
    !! @ref global
    !! @ingroup group_global
    type, public :: global_settings
        private
        type(macro), allocatable, public    :: macros(:)        !< List of global macros
        type(string), allocatable, public   :: undef(:)         !< List of undefined macros
        type(string), allocatable, public   :: includedir(:)    !< List of include directories
        logical, public                     :: expand_macros = .true.   !< Boolean controlling the macro expansion. The macros are expanded by default.
        logical, public                     :: exlude_comments = .false.    !< Boolean controlling the inclusion/exclusion of comments. The comments are kept by default.
        logical, public                     :: implicit_continuation = .false. !< Boolean controlling implicit continuation line.
        logical, public                     :: line_break = .false. !< Boolean controlling line break with double backslash.
        logical, public                     :: extra_macros = .false. !< Boolean controlling extra (non-standard macro definitions: __FILENAME__, __TIMESTAMP__).
    end type

    !> @brief The single global instance used throughout fpx
    !! Initialized automatically with sensible defaults values.
    !! @ingroup group_global
    type(global_settings), public :: global
    
end module
