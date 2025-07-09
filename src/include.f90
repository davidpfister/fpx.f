module fpx_include
    use fpx_constants
    use fpx_logging

    implicit none; private

    public :: handle_include

    interface
        subroutine read_line(line, output_unit, filename, line_num)
            character(*), intent(in)    :: line
            integer, intent(in)         :: output_unit
            character(*), intent(in)    :: filename
            integer, intent(in)         :: line_num
        end subroutine
    end interface

contains

    recursive subroutine handle_include(line, output_unit, parent_file, line_num, process_line)
        character(*), intent(in)    :: line
        integer, intent(in)         :: output_unit
        character(*), intent(in)    :: parent_file
        integer, intent(in)         :: line_num
        procedure(read_line)        :: process_line
        !private
        character(MAX_LINE_LEN) :: include_file, buffer
        integer :: input_unit, ios
        logical :: in_continuation

        include_file = trim(adjustl(line(8:)))
        if (include_file(1:1) == '"' .or. include_file(1:1) == '<') then
            include_file = include_file(2:index(include_file, '"') - 1)
            if (include_file(1:1) == '<') include_file = include_file(2:index(include_file, '>') - 1)
        end if

        open (newunit=input_unit, file=include_file, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            if (verbose) print *, "Error: Cannot open include file '", trim(include_file), "' at ", trim(parent_file), ":", line_num
            return
        end if

        in_continuation = .false.
        buffer = ''
        do
            read (input_unit, '(A)', iostat=ios) buffer
            if (ios /= 0) exit
            if (in_continuation) then
                buffer = trim(buffer)//trim(adjustl(buffer))
            else
                buffer = trim(adjustl(buffer))
            end if
            if (len_trim(buffer) > 0 .and. buffer(len_trim(buffer):len_trim(buffer)) == '\') then
                in_continuation = .true.
                buffer = buffer(:len_trim(buffer) - 1)
                cycle
            else
                in_continuation = .false.
            end if
            call process_line(buffer, output_unit, include_file, line_num)
        end do

        close (input_unit)
    end subroutine
end module
