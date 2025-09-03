!The directory containing the first source file.
!The current working directory where the compilation is taking place (if different from the above directory).
!Any directory or directories specified using the I option. If multiple directories are specified, they are searched in the order specified on the command line, from left to right.
!On Linux* systems, any directories indicated using environment variable CPATH. On Windows* systems, any directories indicated using environment variable INCLUDE.
module fpx_include
    use fpx_constants
    use fpx_logging
    use fpx_path
    use fpx_macro

    implicit none; private

    public :: handle_include

    interface
        function read_line(line, ounit, filename, iline, macros, stitch) result(res)
            import macro
            character(*), intent(in)                :: line
            integer, intent(in)                     :: ounit
            character(*), intent(in)                :: filename
            integer, intent(in)                     :: iline
            type(macro), allocatable, intent(inout) :: macros(:)
            logical, intent(out)                    :: stitch
            character(:), allocatable   :: res
        end function
    end interface

contains

    recursive subroutine handle_include(line, ounit, parent_file, iline, process_line, macros)
        character(*), intent(in)                :: line
        integer, intent(in)                     :: ounit
        character(*), intent(in)                :: parent_file
        integer, intent(in)                     :: iline
        procedure(read_line)                    :: process_line
        type(macro), allocatable, intent(inout) :: macros(:) 
        !private
        character(:), allocatable :: include_file
        character(MAX_LINE_LEN) :: buffer
        character(:), allocatable :: dir, ifile, res
        integer :: icontinuation, input_unit, ios, pos
        logical :: in_continuation, exists, stitch

        dir = dirpath(parent_file)
        icontinuation = 1
        pos = index(line,'include') + len('include')
        include_file = trim(adjustl(line(pos:)))
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

        open (newunit=input_unit, file=include_file, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            if (verbose) print *, "Error: Cannot open include file '", trim(include_file), "' at ", trim(parent_file), ":", iline
            return
        end if

        in_continuation = .false.
        buffer = ''
        do
            read (input_unit, '(A)', iostat=ios) buffer
            if (ios /= 0) exit
            
            if (in_continuation) then
                buffer = buffer(:icontinuation)//trim(adjustl(buffer))
            else
                buffer = trim(adjustl(buffer))
            end if
            
            if (len_trim(buffer) == 0) cycle
            
            if (verify(buffer(len_trim(buffer):len_trim(buffer)), '\') == 0) then
                if (buffer(len_trim(buffer) - 1:len_trim(buffer)) == '\\') then
                    in_continuation = .true.
                    buffer = buffer(:len_trim(buffer) - 2)//new_line('A') ! Strip '\\'
                    icontinuation = len_trim(buffer)
                else
                    in_continuation = .true.
                    icontinuation = len_trim(buffer) - 1
                    buffer = buffer(:icontinuation)
                end if
                cycle
            else
                in_continuation = .false.
                res = process_line(buffer, ounit, include_file, iline, macros, stitch)
                write (ounit, '(A)') trim(adjustl(res))
            end if
        end do

        close (input_unit)
    end subroutine
end module
