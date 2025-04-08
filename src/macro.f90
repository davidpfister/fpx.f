module fpx_macro
    use fpx_constants

    implicit none; private

    public :: expand_macros,    &
              is_defined

    type, public :: macro_t
        character(:), allocatable               :: name
        character(:), allocatable               :: value
        character(MAX_LINE_LEN), allocatable    :: params(:)
        integer                                 :: num_params
        logical                                 :: is_variadic
    end type macro_t

    contains

    recursive function expand_macros(line, macros) result(expanded)
        character(*), intent(in)    :: line
        type(macro_t), intent(in)   :: macros(:)
        character(:), allocatable   :: expanded
        !private
        character(:), allocatable   :: args_str, temp, va_args, token1, token2
        character(MAX_LINE_LEN)     :: arg_values(MAX_PARAMS)
        integer :: i, pos, start, end_pos, paren_level, arg_start, arg_count, j, macro_start, macro_end, k, token1_start
        logical :: param_used(MAX_PARAMS) ! Track which parameters are consumed by ##

        expanded = line
        print *, "Initial expanded: '", trim(expanded), "'"
        do i = 1, size(macros)
            pos = 1
            if (.not. allocated(macros(i)%name)) continue
            do while (pos > 0)
                pos = index(expanded, trim(macros(i)%name))
                if (pos > 0) then
                    macro_start = pos
                    start = pos + len_trim(macros(i)%name)
                    if (macros(i)%num_params > 0 .or. macros(i)%is_variadic) then
                        if (start <= len(expanded) .and. expanded(start:start) == '(') then
                            paren_level = 1
                            arg_start = start + 1
                            arg_count = 0
                            j = arg_start
                            do while (j <= len(expanded) .and. paren_level > 0)
                                if (expanded(j:j) == '(') paren_level = paren_level + 1
                                if (expanded(j:j) == ')') paren_level = paren_level - 1
                                if (paren_level == 1 .and. expanded(j:j) == ',' .or. paren_level == 0) then
                                    if (arg_count < MAX_PARAMS) then
                                        arg_count = arg_count + 1
                                        arg_values(arg_count) = trim(adjustl(expanded(arg_start:j - 1)))
                                        arg_start = j + 1
                                    end if
                                end if
                                j = j + 1
                            end do
                            macro_end = j - 1
                            args_str = expanded(start:macro_end)
                            print *, "Expanding macro: ", trim(macros(i)%name), ", args: ", trim(args_str)
                            temp = trim(macros(i)%value)

                            if (macros(i)%is_variadic) then
                                if (arg_count < macros(i)%num_params) then
                                    print *, "Error: Too few arguments for macro ", trim(macros(i)%name)
                                    cycle
                                end if
                                va_args = ''
                                do j = macros(i)%num_params + 1, arg_count
                                    if (j > macros(i)%num_params + 1) va_args = va_args//', '
                                    va_args = va_args//trim(arg_values(j))
                                end do
                                print *, "__VA_ARGS__: '", trim(va_args), "'"
                            else if (arg_count /= macros(i)%num_params) then
                                print *, "Error: Incorrect number of arguments for macro ", trim(macros(i)%name)
                                cycle
                            end if

                            ! Initialize param_used array
                            param_used = .false.

                            ! Handle concatenation (##) first with immediate substitution
                            pos = 1
                            do while (pos > 0)
                                pos = index(temp, '##')
                                if (pos > 0) then
                                    ! Find token1 (before ##)
                                    k = pos - 1
                                    do while (k > 0 .and. temp(k:k) == ' ')
                                        k = k - 1
                                    end do
                                    if (k <= 0) then
                                        print *, "Error: No token before ##"
                                        cycle
                                    end if
                                    end_pos = k
                                    k = end_pos
                                    do while (k > 0 .and. temp(k:k) /= ' ')
                                        k = k - 1
                                    end do
                                    token1_start = k + 1
                                    token1 = trim(temp(token1_start:end_pos))
                                    ! Substitute token1 and mark as used
                                    do j = 1, macros(i)%num_params
                                        if (trim(token1) == trim(macros(i)%params(j))) then
                                            token1 = trim(arg_values(j))
                                            param_used(j) = .true.
                                            exit
                                        end if
                                    end do

                                    ! Find token2 (after ##)
                                    k = pos + 2
                                    do while (k <= len(temp) .and. temp(k:k) == ' ')
                                        k = k + 1
                                    end do
                                    if (k > len(temp)) then
                                        print *, "Error: No token after ##"
                                        cycle
                                    end if
                                    start = k
                                    do while (k <= len(temp) .and. temp(k:k) /= ' ')
                                        k = k + 1
                                    end do
                                    token2 = trim(temp(start:k - 1))
                                    ! Substitute token2 and mark as used
                                    do j = 1, macros(i)%num_params
                                        if (trim(token2) == trim(macros(i)%params(j))) then
                                            token2 = trim(arg_values(j))
                                            param_used(j) = .true.
                                            exit
                                        end if
                                    end do

                                    ! Concatenate, replacing the full 'token1 ## token2' pattern
                                    temp = trim(temp(:token1_start - 1)//trim(token1)//trim(token2)//trim(temp(k:)))
                                    print *, "Concatenated '", trim(token1), "' and '", trim(token2), "' to '", &
                                             trim(token1)//trim(token2), "', temp: '", trim(temp), "'"
                                end if
                            end do

                            ! Handle stringification (#param)
                            do j = 1, macros(i)%num_params
                                pos = 1
                                do while (pos > 0)
                                    pos = index(temp, '#'//trim(macros(i)%params(j)))
                                    if (pos > 0) then
                                        start = pos + 1 + len_trim(macros(i)%params(j))
                                        temp = trim(temp(:pos - 1)//'"'//trim(arg_values(j))//'"'//trim(temp(start:)))
                                        print *, "Stringified param ", j, ": '", trim(macros(i)%params(j)), & 
                                                 "' to '", trim(arg_values(j)), "', temp: '", trim(temp), "'"
                                        param_used(j) = .true.
                                    end if
                                end do
                            end do

                            ! Substitute regular parameters (only if not used by ## or #)
                            do j = 1, macros(i)%num_params
                                if (.not. param_used(j)) then
                                    pos = 1
                                    do while (pos > 0)
                                        pos = index(temp, trim(macros(i)%params(j)))
                                        if (pos > 0) then
                                            start = pos + len_trim(macros(i)%params(j))
                                            temp = trim(temp(:pos - 1)//trim(arg_values(j))//trim(temp(start:)))
                                            print *, "Substituted param ", j, ": '", trim(macros(i)%params(j)), &
                                                     "' with '", trim(arg_values(j)), "', temp: '", trim(temp), "'"
                                        end if
                                    end do
                                end if
                            end do

                            ! Substitute __VA_ARGS__
                            if (macros(i)%is_variadic) then
                                pos = 1
                                do while (pos > 0)
                                    pos = index(temp, '__VA_ARGS__')
                                    if (pos > 0) then
                                        start = pos + 10
                                   if (start < len(temp) .and. temp(start:start) == '_' .and. temp(start + 1:start + 1) == ')') then
                                            temp = trim(temp(:pos - 1)//trim(va_args)//')')
                                        else
                                            temp = trim(temp(:pos - 1)//trim(va_args)//trim(temp(start:)))
                                        end if
                                        print *, "Substituted __VA_ARGS__ with '", trim(va_args), "', temp: '", trim(temp), "'"
                                    end if
                                end do
                            end if

                            print *, "Before recursive call, temp: '", trim(temp), "'"
                            temp = expand_macros(temp, macros) ! Only for nested macros
                            print *, "After recursive call, temp: '", trim(temp), "'"
                            print *, "Prefix: '", trim(expanded(:macro_start - 1)), "'"
                            print *, "Temp: '", trim(temp), "'"
                            print *, "Suffix: '", trim(expanded(macro_end + 1:)), "'"
                            expanded = trim(expanded(:macro_start - 1)//trim(temp)//expanded(macro_end + 1:))
                            print *, "After substitution, expanded: '", trim(expanded), "'"
                        end if
                    else
                        temp = trim(macros(i)%value)
                        macro_end = start - 1
                        expanded = trim(expanded(:macro_start - 1)//trim(temp)//expanded(macro_end + 1:))
                        expanded = expand_macros(expanded, macros)
                        print *, "Simple macro expanded: '", trim(expanded), "'"
                    end if
                end if
            end do
        end do
    end function

    logical function is_defined(name, macros) result(res)
        character(len=*), intent(in)    :: name
        type(macro_t), intent(in)       :: macros(:)
        integer :: i
        res = .false.
        do i = 1, size(macros)
            if (allocated(macros(i)%name)) then
                if (trim(macros(i)%name) == trim(name)) then
                    res = .true.
                    exit
                end if
            end if
        end do
    end function

end module
