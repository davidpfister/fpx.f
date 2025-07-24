!> @brief Module defining constants for the FPX library
!!        This module provides a set of public integer constants used to enforce limits
!!        in the FPX library, such as maximum line lengths, depths, and parameter counts.
module fpx_constants
    implicit none; private

    public :: starts_with,  &
              head,         &
              tail

    !> @brief Maximum allowed line length in characters, set to 1024 to handle large input strings or files
    integer, parameter, public :: MAX_LINE_LEN = 1024

    !> @brief Maximum nesting depth for structures, set to 50 to balance flexibility and stack safety
    integer, parameter, public :: MAX_DEPTH = 50

    !> @brief Maximum nesting depth for conditional statements, set to 50 to prevent overly complex logic
    integer, parameter, public :: MAX_COND_DEPTH = 50

    !> @brief Maximum number of tokens per line, set to 100 for efficient tokenization
    integer, parameter, public :: MAX_TOKENS = 100

    !> @brief Maximum number of parameters in a signature, set to 10 to ensure manageable interfaces
    integer, parameter, public :: MAX_PARAMS = 10
            
contains

    logical function starts_with(str, arg1, idx) result(res)
        character(*), intent(in) :: str
        character(*), intent(in) :: arg1
        integer, intent(out), optional :: idx
        !private
        integer :: i
        
        i = index(trim(adjustl(str)), trim(arg1))
        res = (i == 1)
        if (present(idx)) idx = i
    end function
    
    character function head(str) result(res)
        character(*), intent(in) :: str
        
        res = ' '
        if (len_trim(str) == 0) return
        
        res = str(1:1)
    end function
    
    character function tail(str) result(res)
        character(*), intent(in) :: str
        !private
        integer :: n
        
        res = ' '; n = len_trim(str)
        if (n == 0) return
        
        res = str(n:n)
    end function

end module
