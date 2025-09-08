!The directory containing the first source file.
!The current working directory where the compilation is taking place (if different from the above directory).
!Any directory or directories specified using the I option. If multiple directories are specified, they are searched in the order specified on the command input, from left to right.
!On Linux* systems, any directories indicated using environment variable CPATH. On Windows* systems, any directories indicated using environment variable INCLUDE.
module fpx_include
    use iso_fortran_env, only : iostat_end
    use fpx_constants
    use fpx_logging
    use fpx_path
    use fpx_string
    use fpx_macro

    implicit none; private

    public :: handle_include

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

    recursive subroutine handle_include(input, ounit, parent_file, iline, preprocess, macros)
        character(*), intent(in)                :: input
        integer, intent(in)                     :: ounit
        character(*), intent(in)                :: parent_file
        integer, intent(in)                     :: iline
        procedure(read_unit)                    :: preprocess
        type(macro), allocatable, intent(inout) :: macros(:)
        !private
        character(:), allocatable :: include_file
        character(:), allocatable :: dir, ifile
        integer :: iunit, ierr, pos
        logical :: exists

        dir = dirpath(parent_file)
        pos = index(input,'include') + len('include')
        include_file = trim(adjustl(input(pos:)))
        if (include_file(1:1) == '"') then
            include_file = include_file(2:index(include_file(2:), '"'))
        else if (include_file(1:1) == '<') then
            include_file = include_file(2:index(include_file(2:), '>'))
        end if
        
        ifile = join(dir, include_file)
        inquire(file=ifile, exist=exists)
        if (exists) then
            include_file = ifile
        else
            ifile = join(cwd(), include_file)
            inquire(file=ifile, exist=exists)
            if (exists) then
                include_file = ifile
            else
                if (verbose) print *, "Error: Cannot find include file '", trim(include_file), "' at ", trim(parent_file), ":", iline
                return
            end if
        end if

        open (newunit=iunit, file=include_file, status='old', action='read', iostat=ierr)
        if (ierr /= 0) then
            if (verbose) print *, "Error: Cannot open include file '", trim(include_file), "' at ", trim(parent_file), ":", iline
            return
        end if
        
        call preprocess(iunit, ounit, macros, .true.)
        close (iunit)
    end subroutine
end module
