module fpx_define
    use fpx_constants
    use fpx_macro, only: macro_t

    implicit none; private

    public :: handle_define,    &
              handle_undef

    contains

    subroutine handle_define(line, num_macros, macros)
        character(*), intent(in)                    :: line
        integer, intent(inout)                      :: num_macros
        type(macro_t), allocatable, intent(inout)   :: macros(:)
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
                    print *, "Param ", i, ": '", trim(macros(num_macros)%params(i)), &
                             "', length = ", len_trim(macros(num_macros)%params(i))
                    i = i + 1
                    pos = pos + 1
                end do
                macros(num_macros)%num_params = param_count
                print *, "Defined variadic macro: ", trim(name), &
                         "(", (trim(macros(num_macros)%params(i))//", ", i=1,param_count), "...) = ", trim(val)
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
                    print *, "Param ", i, ": '", trim(macros(num_macros)%params(i)), &
                             "', length = ", len_trim(macros(num_macros)%params(i))
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

    subroutine handle_undef(line, num_macros, macros)
        character(*), intent(in)                    :: line
        integer, intent(inout)                      :: num_macros
        type(macro_t), allocatable, intent(inout)   :: macros(:)
        !private
        type(macro_t), allocatable :: temp_macros(:)
        character(MAX_LINE_LEN) :: name, temp
        integer :: i

        temp = trim(adjustl(line))
        temp = trim(adjustl(temp(7:)))  ! Skip '#undef'
        name = trim(temp)
        do i = 1, num_macros
            if (trim(macros(i)%name) == name) then
                print *, "Undefining macro: ", trim(name)
                if (allocated(macros(i)%params)) deallocate(macros(i)%params)
                if (num_macros > 1) then
                    macros(i:num_macros-1) = macros(i+1:num_macros)
                    allocate(temp_macros(num_macros-1))
                    temp_macros = macros(:num_macros-1)
                    deallocate(macros)
                    call move_alloc(temp_macros, macros)
                else
                    deallocate(macros); allocate(macros(0))
                end if
                num_macros = num_macros - 1
                exit
            end if
        end do
        if (i > num_macros) then
            print *, "Warning: Macro ", trim(name), " not found for #undef"
        end if
    end subroutine
end module