module fpx_constants
    implicit none; private

    integer, parameter, public :: MAX_LINE_LEN = 1024
    integer, parameter, public :: MAX_DEPTH = 50
    integer, parameter, public :: MAX_COND_DEPTH = 50
    integer, parameter, public :: MAX_TOKENS = 100
    integer, parameter, public :: MAX_PARAMS = 10
end module