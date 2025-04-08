module fpx_parser
    use fpx_constants
    use fpx_macro
    use fpx_conditional
    use fpx_token

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

            ! Check for line break with '\\'
          if (len_trim(continued_line) > 1 .and. continued_line(len_trim(continued_line) - 1:len_trim(continued_line)) == '\\') then
                in_continuation = .true.
                continued_line = continued_line(:len_trim(continued_line) - 2)//new_line('A') ! Strip '\\'
                write (output_unit, '(A)') '' ! Add extra newline for line break
                ! Check for line continuation with '\'
          else if (len_trim(continued_line) > 0 .and. continued_line(len_trim(continued_line):len_trim(continued_line)) == '\') then
                in_continuation = .true.
                continued_line = continued_line(:len_trim(continued_line) - 1)
                cycle
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

    subroutine process_line(line, output_unit, filename, line_num)
        character(*) :: line
        integer, intent(in) :: output_unit, line_num
        character(*), intent(in) :: filename
        character(:), allocatable :: trimmed_line, expanded_line
        logical :: active

        trimmed_line = trim(adjustl(line))
        if (len_trim(trimmed_line) == 0) return

        active = is_active()
        print *, "Processing line ", line_num, ": '", trim(trimmed_line), "'"
        print *, "is_active() = ", active, ", cond_depth = ", cond_depth
        if (trimmed_line(1:1) == '#') then
            if (starts_with(trimmed_line, '#define') .and. active) then
                call handle_define(trimmed_line)
            else if (starts_with(trimmed_line, '#include') .and. active) then
                call handle_include(trimmed_line, output_unit, filename, line_num)
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

    subroutine handle_define(line)
        character(*), intent(in) :: line
        !private
        character(MAX_LINE_LEN) :: name, temp
        character(:), allocatable :: val
        integer :: pos, paren_start, paren_end, i, param_count
        type(macro_t), allocatable :: temp_macros(:)

        pos = index(line, ' ')
        temp = trim(adjustl(line(pos + 1:)))
        paren_start = index(temp, '(')
        if (paren_start > 0) then
            name = trim(temp(:paren_start - 1))
            paren_end = index(temp, ')')
            if (paren_end == 0) then
                print *, "Error: Unclosed parenthesis in macro definition: ", trim(line)
                return
            end if
            val = trim(adjustl(temp(paren_end + 1:)))
            print *, "Raw value before allocation: ", val, ", length = ", len(val)

            temp = temp(paren_start + 1:paren_end - 1)
            param_count = 0
            pos = 1
            do while (pos <= len_trim(temp))
                if (temp(pos:pos) == ',') then
                    param_count = param_count + 1
                end if
                pos = pos + 1
            end do
            if (len_trim(temp) > 0) param_count = param_count + 1

            if (.not. allocated(macros)) allocate (macros(0))
            num_macros = num_macros + 1
            if (num_macros > size(macros)) then
                allocate (temp_macros(num_macros))
                temp_macros(1:size(macros)) = macros
                call move_alloc(temp_macros, macros)
            end if
            macros(num_macros)%name = name
            allocate (character(len_trim(val)) :: macros(num_macros)%value)
            macros(num_macros)%value = val

            if (index(temp, '...') > 0) then
                macros(num_macros)%is_variadic = .true.
                param_count = param_count - 1
                allocate (macros(num_macros)%params(param_count))
                pos = 1
                i = 1
                do while (pos <= len_trim(temp) .and. i <= param_count)
                    do while (pos <= len_trim(temp) .and. temp(pos:pos) == ' ')
                        pos = pos + 1
                    end do
                    if (pos > len_trim(temp)) exit
                    paren_start = pos
                    do while (pos <= len_trim(temp) .and. temp(pos:pos) /= ',')
                        pos = pos + 1
                    end do
                    macros(num_macros)%params(i) = temp(paren_start:pos - 1)
             print *, "Param ", i, ": '", trim(macros(num_macros)%params(i)), "', length = ", len_trim(macros(num_macros)%params(i))
                    i = i + 1
                    pos = pos + 1
                end do
                macros(num_macros)%num_params = param_count
        print *, "Defined variadic macro: ", trim(name), "(", (trim(macros(num_macros)%params(i))//", ", i=1,param_count), "...) = ", trim(val)
            else
                macros(num_macros)%is_variadic = .false.
                allocate (macros(num_macros)%params(param_count))
                pos = 1
                i = 1
                do while (pos <= len_trim(temp) .and. i <= param_count)
                    do while (pos <= len_trim(temp) .and. temp(pos:pos) == ' ')
                        pos = pos + 1
                    end do
                    if (pos > len_trim(temp)) exit
                    paren_start = pos
                    do while (pos <= len_trim(temp) .and. temp(pos:pos) /= ',' .and. temp(pos:pos) /= ' ')
                        pos = pos + 1
                    end do
                    macros(num_macros)%params(i) = temp(paren_start:pos - 1)
             print *, "Param ", i, ": '", trim(macros(num_macros)%params(i)), "', length = ", len_trim(macros(num_macros)%params(i))
                    i = i + 1
                    if (pos <= len_trim(temp) .and. temp(pos:pos) == ',') pos = pos + 1
                end do
                macros(num_macros)%num_params = param_count
                print *, "Defined macro: ", trim(name), "(", (trim(macros(num_macros)%params(i))//", ", i=1, param_count - 1), &
                    trim(macros(num_macros)%params(param_count)), ") = ", trim(val)
            end if
        else
            pos = index(temp, ' ')
            if (pos > 0) then
                name = trim(temp(:pos - 1))
                val = trim(adjustl(temp(pos + 1:)))
            else
                name = trim(temp)
                val = ''
            end if
            if (.not. allocated(macros)) allocate (macros(0))
            num_macros = num_macros + 1
            if (num_macros > size(macros)) then
                allocate (temp_macros(num_macros))
                temp_macros(1:size(macros)) = macros
                call move_alloc(temp_macros, macros)
            end if
            macros(num_macros)%name = name
            allocate (character(len_trim(val)) :: macros(num_macros)%value)
            macros(num_macros)%value = val
            macros(num_macros)%num_params = 0
            macros(num_macros)%is_variadic = .false.
            print *, "Defined macro: ", trim(name), " = ", trim(val)
        end if
    end subroutine

    recursive subroutine handle_include(line, output_unit, parent_file, line_num)
        character(*), intent(in) :: line, parent_file
        integer, intent(in) :: output_unit, line_num
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
            print *, "Error: Cannot open include file '", trim(include_file), "' at ", trim(parent_file), ":", line_num
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
