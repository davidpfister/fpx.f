module fpx_parser
    use fpx_constants
    use fpx_macro
    use fpx_conditional
    use fpx_define
    use fpx_include

    implicit none; private

    public :: preprocess_file

    type(macro_t), allocatable :: macros(:)
    integer :: num_macros = 0

contains

    subroutine preprocess_file(input_file, output_file)
        character(*), intent(in) :: input_file, output_file
        character(MAX_LINE_LEN) :: line, continued_line
        integer :: input_unit, output_unit, ios, line_num
        logical :: in_continuation

        open (newunit=input_unit, file=input_file, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, "Error opening input file: ", trim(input_file)
            return
        end if
        open (newunit=output_unit, file=output_file, status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            print *, "Error opening output file: ", trim(output_file)
            close (input_unit)
            return
        end if

        if (allocated(macros)) deallocate (macros)
        allocate (macros(0))
        cond_depth = 0
        cond_stack(1)%active = .true.
        cond_stack(1)%has_met = .false.

        line_num = 0
        in_continuation = .false.
        continued_line = ''
        do
            read (input_unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            line_num = line_num + 1

            if (in_continuation) then
                continued_line = trim(continued_line)//trim(adjustl(line))
            else
                continued_line = trim(adjustl(line))
            end if
            
            if (len_trim(continued_line) == 0) cycle

            ! Check for line continuation with '\'
            if (continued_line(len_trim(continued_line):len_trim(continued_line)) == '\') then
                ! Check for line break with '\\'
                if (continued_line(len_trim(continued_line) - 1:len_trim(continued_line)) == '\\') then
                    in_continuation = .true.
                    continued_line = continued_line(:len_trim(continued_line) - 2)//new_line('A') ! Strip '\\'
                    cycle
                else
                    in_continuation = .true.
                    continued_line = continued_line(:len_trim(continued_line) - 1)
                    cycle
                end if
            else
                in_continuation = .false.
                call process_line(continued_line, output_unit, input_file, line_num)
            end if
        end do

        if (cond_depth > 0) then
            print *, "Error: Unclosed conditional block at end of file ", trim(input_file)
        end if

        close (input_unit)
        close (output_unit)
        deallocate (macros)
    end subroutine

    recursive subroutine process_line(line, output_unit, filename, line_num)
        character(*), intent(in)    :: line
        integer, intent(in)         :: output_unit
        character(*), intent(in)    :: filename
        integer, intent(in)         :: line_num
        !private
        character(:), allocatable :: trimmed_line, expanded_line
        logical :: active

        trimmed_line = trim(adjustl(line))
        if (len_trim(trimmed_line) == 0) return

        active = is_active()
        print *, "Processing line ", line_num, ": '", trim(trimmed_line), "'"
        print *, "is_active() = ", active, ", cond_depth = ", cond_depth
        if (trimmed_line(1:1) == '#') then
            if (starts_with(trimmed_line, '#define') .and. active) then
                call handle_define(trimmed_line, num_macros, macros)
            else if (starts_with(trimmed_line, '#undef') .and. active) then
                call handle_undef(trimmed_line, num_macros, macros)
            else if (starts_with(trimmed_line, '#include') .and. active) then
                call handle_include(trimmed_line, output_unit, filename, line_num, process_line)
            else if (starts_with(trimmed_line, '#if')) then
                call handle_if(trimmed_line, filename, line_num, macros)
            else if (starts_with(trimmed_line, '#ifdef')) then
                call handle_ifdef(trimmed_line, filename, line_num, macros)
            else if (starts_with(trimmed_line, '#ifndef')) then
                call handle_ifndef(trimmed_line, filename, line_num, macros)
            else if (starts_with(trimmed_line, '#elif')) then
                call handle_elif(trimmed_line, filename, line_num, macros)
            else if (starts_with(trimmed_line, '#else')) then
                call handle_else(filename, line_num)
            else if (starts_with(trimmed_line, '#endif')) then
                call handle_endif(filename, line_num)
            end if
        else if (active) then
            expanded_line = expand_macros(trimmed_line, macros)
            print *, "Writing to output: '", trim(expanded_line), "'"
            write (output_unit, '(A)') trim(expanded_line)
        end if
    end subroutine

end module
