!The directory containing the first source file.
!The current working directory where the compilation is taking place (if different from the above directory).
!Any directory or directories specified using the I option. If multiple directories are specified, they are searched in the order specified on the command line, from left to right.
!On Linux* systems, any directories indicated using environment variable CPATH. On Windows* systems, any directories indicated using environment variable INCLUDE.
module fpx_include
    use fpx_constants
    use fpx_logging
    use fpx_path

    implicit none; private

    public :: handle_include

    interface
        subroutine read_line(line, output_unit, filename, iline)
            character(*), intent(in)    :: line
            integer, intent(in)         :: output_unit
            character(*), intent(in)    :: filename
            integer, intent(in)         :: iline
        end subroutine
    end interface

contains

    recursive subroutine handle_include(line, output_unit, parent_file, iline, process_line)
        character(*), intent(in)    :: line
        integer, intent(in)         :: output_unit
        character(*), intent(in)    :: parent_file
        integer, intent(in)         :: iline
        procedure(read_line)        :: process_line
        !private
        character(MAX_LINE_LEN) :: include_file, buffer
        character(:), allocatable :: dir, ifile
        integer :: icontinuation, input_unit, ios, pos
        logical :: in_continuation, exists

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
                call process_line(buffer, output_unit, include_file, iline)
            end if
        end do

        close (input_unit)
    end subroutine
end module
