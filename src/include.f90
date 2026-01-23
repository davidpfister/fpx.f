!> @file
!! @defgroup group_include Include
!! Include file handling and resolution for the fpx Fortran preprocessor
!!
!! This module implements robust and standard-compliant processing of `#include` directives
!! with full support for:
!! - Both forms: `#include "file.h"` (local/user) and `#include <file.h>` (system)
!! - Relative paths resolved against the directory of the parent source file
!! - Search in user-defined include directories (`global%includedir`)
!! - Fallback to current working directory
!! - Proper error reporting with file name and line number context
!! - Recursion safety through integration with the main preprocessor loop
!! - Seamless integration via the abstract `preprocess` procedure pointer
!!
!! The routine correctly strips quotes or angle brackets, performs path resolution,
!! checks file existence, opens the file, and recursively invokes the main preprocessing
!! engine on the included content using the same macro environment.
!!
!! <h2  class="groupheader">Examples</h2>
!!
!! 1. Include a local header from the same directory using quote or angle brackets:
!! @code{.f90}
!!    #include <config.h>
!!    !> fpx will look for ./config.h relative to the current source file
!! @endcode
!!
!! 2. Using from the driver program (adding include paths):
!! @code{.f90}
!!    global%includedir = ['/usr/include', './include', './headers']
!!    call preprocess('main.F90', 'main.f90')
!!    !> All #include <...> will search these directories in order
!! @endcode
!!
!! 3. Verbose error reporting when a file is not found:
!! @code{.txt}
!!    $ fpx -v src/utils.F90
!!    Error: Cannot find include file 'missing.h' at src/utils.F90:27
!! @endcode
module fpx_include
    use iso_fortran_env, only : iostat_end
    use fpx_constants
    use fpx_logging
    use fpx_path
    use fpx_string
    use fpx_macro
    use fpx_global

    implicit none; private

    public :: handle_include

    !> Abstract interface for the main preprocessing routine (used for recursion)
    !! Allows handle_include to recursively call the top-level preprocess_unit routine
    !! without creating circular module dependencies.
    !!
    !! @b Remarks
    !! @ingroup group_include
    interface
        subroutine read_unit(iunit, ounit, macros, from_include)
            import macro
            integer, intent(in)                     :: iunit
            integer, intent(in)                     :: ounit
            type(macro), allocatable, intent(inout) :: macros(:)
            logical, intent(in)                     :: from_include
        end subroutine
    end interface

contains

    !> Process a #include directive encountered during preprocessing
    !! Resolves the include file name (quoted or angle-bracketed), searches for the file
    !! using standard rules (parent directory first, then global include paths, then cwd),
    !! opens it, and recursively preprocesses its contents into the output unit.
    !! @param[in] input        Full line containing the #include directive
    !! @param[in] ounit        Output unit where preprocessed content is written
    !! @param[in] parent_file  Path of the file containing the #include
    !! @param[in] iline        Line number in parent file (for error messages)
    !! @param[in] preprocess   Procedure pointer to the main line-by-line preprocessor
    !! @param[inout] macros    Current macro table (shared across recursion levels)
    !! @param[in] token        Usually 'INCLUDE' – the directive keyword
    !!
    !! @b Remarks
    !! @ingroup group_include
    recursive subroutine handle_include(input, ounit, parent_file, iline, preprocess, macros, token)
        character(*), intent(in)                :: input
        integer, intent(in)                     :: ounit
        character(*), intent(in)                :: parent_file
        integer, intent(in)                     :: iline
        procedure(read_unit)                    :: preprocess
        type(macro), allocatable, intent(inout) :: macros(:)
        character(*), intent(in)                :: token
        !private
        character(:), allocatable :: include_file
        character(:), allocatable :: dir, ifile
        integer :: i, iunit, ierr, pos
        logical :: exists

        dir = dirpath(parent_file)
        pos = index(uppercase(input), token) + len(token)
        include_file = trim(adjustl(input(pos:)))
        if (include_file(1:1) == '"') then
            include_file = include_file(2:index(include_file(2:), '"'))
        else if (include_file(1:1) == '<') then
            include_file = include_file(2:index(include_file(2:), '>'))
        end if

        ifile = include_file
        if (is_rooted(ifile)) then
            inquire(file=ifile, exist=exists)
            if (exists) then
                include_file = ifile
            else
                if (verbose) print *, "Error: Cannot find include file '", trim(include_file), "' at ", trim(parent_file), ":", &
                        iline
                return
            end if
        else
            ifile = join(dir, include_file)
            inquire(file=ifile, exist=exists)
            if (exists) then
                include_file = ifile
            else
                do i = 1, size(global%includedir)
                    ifile = join(global%includedir(i), include_file)
                    inquire(file=ifile, exist=exists)
                    if (exists) then
                        include_file = ifile
                        exit
                    end if
                end do

                if (.not. exists) then
                    ifile = join(cwd(), include_file)
                    inquire(file=ifile, exist=exists)
                    if (exists) then
                        include_file = ifile
                    else
                        if (verbose) print *, "Error: Cannot find include file '", trim(include_file), "' at ", trim(parent_file), &
                                ":", iline
                        return
                    end if
                end if
            end if
        end if

        open(newunit=iunit, file=include_file, status='old', action='read', iostat=ierr)
        if (ierr /= 0) then
            if (verbose) print *, "Error: Cannot open include file '", trim(include_file), "' at ", trim(parent_file), ":", iline
            return
        end if

        call preprocess(iunit, ounit, macros, .true.)
        close(iunit)
    end subroutine
end module
