!> @defgroup group_logging fpx_logging
!> @brief Global logging and verbosity control for the fpx Fortran preprocessor
!!
!! This tiny but essential module that provides a single public logical flag `verbose`
!! to enable or disable detailed diagnostic output throughout the entire fpx tool
!! (parser, macro expansion, include handling, conditional compilation, etc.).
!!
!! When `verbose = .true.`, the preprocessor prints extensive debugging information
!! such as:
!! - Currently processed file and line number
!! - Active conditional blocks
!! - Macro expansions (before/after)
!! - Include file resolution
!! - Directive handling
!! - Internal state changes
!!
!! This is extremely useful during development, testing, and when debugging complex
!! macro or include issues in large Fortran projects.
!!
!! The flag is intentionally public and module-level so it can be set from anywhere
!! (command-line driver, interactive mode, or library user).
!!
!! @par Examples
!!
!! 1. Enable verbose output from the main program:
!! @code{.f90}
!!    use fpx_logging, only: verbose
!!    use fpx_parser
!!    
!!    verbose = .true.           ! Turn on all diagnostic messages
!!    call preprocess("src/main.F90", "build/main.f90")
!! @endcode
!!
!! 2. Temporarily enable verbosity for a single file processing:
!! @code{.f90}
!!    use fpx_logging, only: verbose
!!    verbose = .true.
!!    call preprocess("debug_this.F90")
!!    verbose = .false.          ! Turn off again
!! @endcode
!!
!! 3. Interactive session with full insight:
!!    $ fpx -v input.F90
!!    !> (implementation sets verbose = .true. from command-line “-v”)
!!    !> You will see every macro expansion, #ifdef state, include path, etc.
!! @{
module fpx_logging
    implicit none; private
    
    !> @brief Master switch for verbose diagnostic output
    !! Default value is `.false.` (quiet mode).
    !! Set to `.true.` to get detailed step-by-step information about
    !! preprocessing actions. Safe to modify at any time – the change takes
    !! effect immediately for all subsequent operations.
    logical, public :: verbose
    
end module
!! @}