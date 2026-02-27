!> @file
!! @defgroup group_constants Constants
!! Module defining constants for the FPX library
!! This module provides a set of public integer constants used to enforce limits
!! in the FPX library, such as maximum line lengths, depths, and parameter counts.
module fpx_constants
    implicit none; private

    !> @brief Maximum allowed line length in characters, set to 1000000 to handle large input strings or files
    !! @ingroup group_constants
    integer, parameter, public :: MAX_LINE_LEN = 4096

    !> @brief Maximum nesting depth for structures, set to 50 to balance flexibility and stack safety
    !! @ingroup group_constants
    integer, parameter, public :: MAX_DEPTH = 50

    !> @brief Maximum nesting depth for conditional statements, set to 50 to prevent overly complex logic
    !! @ingroup group_constants
    integer, parameter, public :: MAX_COND_DEPTH = 50

    !> @brief Maximum number of tokens per line, set to 100 for efficient tokenization
    !! @ingroup group_constants
    integer, parameter, public :: MAX_TOKENS = 100

    !> @brief Maximum number of parameters in a signature, set to 10 to ensure manageable interfaces
    !! @ingroup group_constants
    integer, parameter, public :: MAX_PARAMS = 10

    !> @brief Maximum chunk size
    !! @ingroup group_constants
    integer, parameter, public :: CHKSIZE = 72
end module
