!> @file
!! @defgroup group_constants Constants
!! Compile-time constants used throughout the fpx preprocessor.
!!
!! This module centralizes all numerical limits and fixed configuration
!! values required by the implementation. These parameters define the
!! maximum sizes of internal buffers and the allowed nesting depth of
!! various preprocessing constructs.
!!
!! The chosen values aim to balance flexibility and robustness:
!!
!! - large enough to accommodate realistic scientific Fortran code,
!! - small enough to avoid excessive memory consumption,
!! - fixed at compile time to simplify implementation and improve performance.
!!
!! The constants are used across multiple modules, including macro
!! expansion, conditional compilation, tokenization, and the extended
!! #for directive implementation.
!!
!! @section constants_examples Examples
!!
!! -# Limiting nested conditional directives:
!!      @code{.f90}
!!      #if A
!!      #if B
!!      #if C
!!      !some code
!!      #endif
!!      #endif
!!      #endif
!!      ...
!!      @endcode
!!
!!      Nesting is permitted up to @link fpx_constants::max_cond_depth MAX_COND_DEPTH @endlink levels.
!!
!! -# Nested #for loops:
!!      @code{.f90}
!!      #for T in [integer, real]
!!      #for K in [32, 64]
!!      !some code
!!      #endfor
!!      #endfor
!!      ...
!!      @endcode
!!
!!      Loop nesting is limited by @link fpx_constants::max_for_depth MAX_FOR_DEPTH @endlink.
!!
!! -# Long macro expansions:
!!      @code{.f90}
!!      #define MESSAGE "very long text ..."
!!      ...
!!      @endcode
!!
!!      Intermediate buffers are sized according to @link fpx_constants::max_line_len MAX_LINE_LEN @endlink.
module fpx_constants
    implicit none; private

    !> Maximum permitted length of an input or generated line. 
    !! 
    !! This limit applies to raw source lines, continued lines, 
    !! and intermediate results produced during macro expansion. 
    !! 
    !! The value should be sufficiently large for practically all 
    !! modern Fortran source files while preventing unbounded memory use. 
    !! 
    !! @ingroup group_constants
    integer, parameter, public :: MAX_LINE_LEN = 4096

    !> Maximum nesting depth of generic parser structures. 
    !! 
    !! Used internally whenever recursive parser constructs require 
    !! bounded stack-like storage. 
    !! 
    !! @ingroup group_constants
    integer, parameter, public :: MAX_DEPTH = 50

    !> Maximum nesting depth of conditional compilation directives. 
    !! 
    !! Applies to constructs such as: 
    !! - `#if` 
    !! - `#ifdef` 
    !! - `#ifndef` 
    !! - `#elif` 
    !! - `#else` 
    !! 
    !! Excessive nesting beyond this limit results in diagnostics. 
    !! 
    !! @ingroup group_constants
    integer, parameter, public :: MAX_COND_DEPTH = 50

    !> Maximum nesting depth of `#for` loops. 
    !! 
    !! Applies to the fpx extension: 
    !! @code 
    !! #for ...
    !! !some code
    !! #endfor
    !! ...
    !! @endcode 
    !! 
    !! Nested loops exceeding this limit generate an error. 
    !! 
    !! @ingroup group_constants
    integer, parameter, public :: MAX_FOR_DEPTH = 50

    !> Maximum number of tokens generated during tokenization. 
    !! 
    !! This value bounds the temporary token buffers used by 
    !! expression parsing and macro processing. 
    !! 
    !! @ingroup group_constants
    integer, parameter, public :: MAX_TOKENS = 100

    !> Maximum number of parameters accepted by a macro definition. 
    !! 
    !! Applies to function-like macros: 
    !! @code 
    !! #define F(a,b,c) ...
    !! ...
    !! @endcode 
    !! 
    !! Variadic arguments count toward this limit. 
    !! 
    !! @ingroup group_constants
    integer, parameter, public :: MAX_PARAMS = 10

    !> Default chunk size used for internal buffering operations. 
    !! 
    !! This value is primarily used when processing data incrementally 
    !! to avoid frequent reallocations. 
    !! 
    !! @ingroup group_constants
    integer, parameter, public :: CHKSIZE = 72
end module
