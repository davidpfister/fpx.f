!> @file
!! @defgroup group_include Include
!! Include file handling and resolution for the fpx Fortran preprocessor
!!
!! This module implements robust and standard-compliant processing of `#include` directives
!! with full support for:
!! - Both forms: `#include "file.h"` (local/user) and `#include <file.h>` (system)
!! - Proper search order: quotes search source dir first, angle brackets skip source dir
!! - Relative paths resolved against the directory of the parent source file
!! - Search in user-defined include directories (`global%includedir`)
!! - Search in system PATH environment variable directories
!! - Fallback to current working directory
!! - Proper error reporting with file name and line number context
!! - Recursion safety through integration with the main preprocessor loop
!! - Seamless integration via the abstract `preprocess` procedure pointer
!!
!! The routine correctly strips quotes or angle brackets, performs path resolution,
!! checks file existence, opens the file, and recursively invokes the main preprocessing
!! engine on the included content using the same macro environment.
!!
!! <h2  class="groupheader">Search Order</h2>
!!
!! For `#include "file"`:
!! 1. Directory of the parent source file
!! 2. Directories specified by -I or -Y options (global%includedir)
!! 3. Directories in INCLUDE environment variable
!! 4. Current working directory
!!
!! For `#include <file>`:
!! 1. Directories specified by -I or -Y options (global%includedir)
!! 2. Directories in INCLUDE environment variable
!! 3. Current working directory
!!
!! <h2  class="groupheader">Examples</h2>
!!
!! 1. Include a local header from the same directory using quotes:
!! @code{.f90}
!!    #include "config.h"
!!    !> fpx will look for ./config.h relative to the current source file first
!! @endcode
!!
!! 2. Include a system header using angle brackets:
!! @code{.f90}
!!    #include <stdlib.h>
!!    !> fpx will skip the source directory and search -I paths, then PATH
!! @endcode
!!
!! 3. Using from the driver program (adding include paths):
!! @code{.f90}
!!    global%includedir = ['/usr/include', './include', './headers']
!!    call preprocess('main.F90', 'main.f90')
!!    !> All #include <...> will search these directories in order
!! @endcode
!!
!! 4. Verbose error reporting when a file is not found:
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

    ! Include directive types
    integer, parameter, private :: INCLUDE_TYPE_SYSTEM = 1  ! < >
    integer, parameter, private :: INCLUDE_TYPE_LOCAL  = 2  ! " "
#ifdef _WIN32
    integer, parameter, private :: MAX_PATH_LEN = 256
#else
    integer, parameter, private :: MAX_PATH_LEN = 4096
#endif
    !> Abstract interface for the main preprocessing routine (used for recursion)
    !! Allows handle_include to recursively call the top-level preprocess_unit routine
    !! without creating circular module dependencies.
    !!
    !! @b Remarks
    !! @ingroup group_include
    interface
        subroutine read_unit(iunit, ounit, macros, from_include)
            import macro
            implicit none
            integer, intent(in)                     :: iunit
            integer, intent(in)                     :: ounit
            type(macro), allocatable, intent(inout) :: macros(:)
            logical, intent(in)                     :: from_include
        end subroutine
    end interface

contains

    !> Process a #include directive encountered during preprocessing
    !! Resolves the include file name (quoted or angle-bracketed), searches for the file
    !! using standard C preprocessor rules:
    !! - Quoted includes search: parent directory, -I paths, PATH, cwd
    !! - Angle bracket includes search: -I paths, PATH, cwd (skips parent directory)
    !! Opens the file and recursively preprocesses its contents into the output unit.
    !!
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
        character(:), allocatable :: sys_paths(:)
        integer :: i, iunit, ierr, pos
        integer :: include_type
        logical :: exists

        ! Extract the directory of the parent file
        dir = dirpath(parent_file)
        ! Find the position after the #include token
        pos = index(uppercase(input), token) + len(token)
        include_file = trim(adjustl(input(pos:)))
        
        ! Determine include type and extract filename
        if (include_file(1:1) == '"') then
            include_type = INCLUDE_TYPE_LOCAL
            include_file = include_file(2:index(include_file(2:), '"'))
        else if (include_file(1:1) == '<') then
            include_type = INCLUDE_TYPE_SYSTEM
            include_file = include_file(2:index(include_file(2:), '>'))
        else
            ! Malformed include directive
            if (verbose) print *, 'Error: Malformed #include directive at ', trim(parent_file), ':', iline
            return
        end if

        ! Handle absolute/rooted paths (same for both types)
        ifile = include_file
        if (is_rooted(ifile)) then
            inquire(file=ifile, exist=exists)
            if (exists) then
                include_file = ifile
            else
                if (verbose) then 
                    print *, "Error: Cannot find include file '", trim(include_file), "' at ", trim(parent_file), ":", iline
                    return
                end if
            end if
        else
            ! Relative path - search according to include type
            exists = .false.
            ! For quoted includes (#include "file"), search parent directory first
            ifile = join(dir, include_file)
            if (include_type == INCLUDE_TYPE_LOCAL) then
                ifile = join(dir, include_file)
                inquire(file=ifile, exist=exists)
                if (exists) then
                    include_file = ifile
                end if
            end if
            
            ! If not found yet, search user-specified include directories (-I paths)
            if (.not. exists .and. allocated(global%includedir)) then
                do i = 1, size(global%includedir)
                    ifile = join(global%includedir(i), include_file)
                    inquire(file=ifile, exist=exists)
                    if (exists) then
                        include_file = ifile
                        exit
                    end if
                end do
             end if
             
            ! If still not found, try the INCLUDE environmental variable
            if (.not. exists) then
                block
                    character(:), allocatable :: ipaths(:)
                    
                    ipaths = get_system_paths()
                    do i = 1, size(ipaths)
                        ifile = join(ipaths(i), include_file)
                        inquire(file=ifile, exist=exists)
                        if (exists) then
                            include_file = ifile
                        end if
                    end do
                end block
            end if
            
            ! If still not found, try current working directory as last resort
            if (.not. exists) then
                ifile = join(cwd(), include_file)
                inquire(file=ifile, exist=exists)
                if (exists) then
                    include_file = ifile
                end if
            end if
            
            ! If file was not found anywhere, report error
            if (.not. exists) then
                if (verbose) print *, "Error: Cannot find include file '", trim(include_file), "' at ", trim(parent_file), ":", iline
                return
            end if
        end if

        ! Open and preprocess the include file
        open(newunit=iunit, file=include_file, status='old', action='read', iostat=ierr)
        if (ierr /= 0) then
            if (verbose) print *, "Error: Cannot open include file '", trim(include_file), "' at ", trim(parent_file), ":", iline
            return
        end if
        
        call preprocess(iunit, ounit, macros, .true.)
        close(iunit)
    end subroutine

    !> Get system include paths from PATH environment variable
    !! Returns an array of directory paths found in PATH
    !! @return Array of path strings, empty if PATH not set
    !!
    !! @b Remarks
    !! @ingroup group_include
    function get_system_paths() result(paths)
        character(:), allocatable :: paths(:)
        !private
        character(:), allocatable :: path_env, tmp(:)
        integer :: lpath, i, n_paths, start_pos, end_pos, count
        character(len=1) :: path_sep
        
#ifdef _WIN32
        path_sep = ';'  ! Windows path separator
#else
        path_sep = ':'  ! Unix/Linux/Mac path separator
#endif
        
        ! Get PATH environment variable length
        call get_environment_variable('INCLUDE', length=lpath)
        if (lpath <= 0) then
            allocate(character(len=0) :: paths(0)); return
        end if
        
        ! Allocate and retrieve PATH value
        allocate(character(len=lpath) :: path_env)
        call get_environment_variable('INCLUDE', value=path_env)
        
        ! Count number of paths (number of separators + 1)
        n_paths = 1
        do i = 1, len(path_env)
            if (path_env(i:i) == path_sep) n_paths = n_paths + 1
        end do
        
        ! Allocate temporary array with maximum size
        allocate(character(len=MAX_PATH_LEN) :: tmp(n_paths))
        
        ! Split INCLUDE into individual directories
        count = 0
        start_pos = 1
        do i = 1, len(path_env) + 1
            if (i > len(path_env) .or. path_env(i:i) == path_sep) then
                if (i > len(path_env)) then
                    end_pos = i - 1
                else
                    end_pos = i - 1
                end if
                
                if (end_pos >= start_pos) then
                    count = count + 1
                    tmp(count) = trim(adjustl(path_env(start_pos:end_pos)))
                end if
                start_pos = i + 1
            end if
        end do
        
        ! Allocate result array with actual count
        if (count > 0) then
            allocate(character(len=MAX_PATH_LEN) :: paths(count))
            paths(:) = tmp(1:count)
        else
            allocate(character(len=0) :: paths(0))
        end if
    end function

end module
