module fpx_macro
    use fpx_constants
    use fpx_logging
    use fpx_path

    implicit none; private

    public :: expand_macros, &
              expand_all, &
              is_defined

    type, public :: macro_t
        character(:), allocatable :: name
        character(:), allocatable :: value
        character(MAX_LINE_LEN), allocatable :: params(:)
        integer :: num_params
        logical :: is_variadic ! New flag for variadic macros
    contains
        procedure, pass(this), private :: set_default
        procedure, pass(this), private :: set_with_value
        generic, public :: set => set_default, set_with_value
    end type
    
    interface macro_t
        module procedure :: macro_new
    end interface

    contains
    
    type(macro_t) function macro_new(name, val) result(that)
        character(*), intent(in)    :: name
        character(*), intent(in)    :: val
        
        that%name = name
        that%value = val
        that%num_params = 0
        that%is_variadic = .false.
    end function
    
    subroutine set_default(this, name)
        class(macro_t), intent(inout)   :: this
        character(*), intent(in)        :: name
        
        this%name = name
        this%value = ''
    end subroutine
    
    subroutine set_with_value(this, name, value)
        class(macro_t), intent(inout)   :: this
        character(*), intent(in)        :: name
        character(*), intent(in)        :: value
        
        this%name = name
        this%value = value
    end subroutine

    function expand_all(line, macros, filepath, iline) result(expanded)
        character(*), intent(in)            :: line
        type(macro_t), intent(in)           :: macros(:)
        character(*), intent(in)            :: filepath
        integer, intent(in)                 :: iline
        character(:), allocatable :: expanded
        !private
        integer :: pos, start, sep, dot

        expanded = expand_macros(line, macros)
        ! Substitute __FILENAME__
        pos = 1
        do while (pos > 0)
            pos = index(expanded, '__FILENAME__')
            if (pos > 0) then
                start = pos + len('__FILENAME__')
                expanded = trim(expanded(:pos - 1)//'"'//filename(filepath, .true.)//'"'//trim(expanded(start:)))
                if (verbose) print *, "Substituted __FILENAME__ with '", trim(filepath), "', expanded: '", trim(expanded), "'"
            end if
        end do
        
        ! Substitute __FILE__ (relative path to working directory)
        pos = 1
        do while (pos > 0)
            pos = index(expanded, '__FILE__')
            if (pos > 0) then
                start = pos + len('__FILE__')
                expanded = trim(expanded(:pos - 1)//'"'//trim(filepath)//'"'//trim(expanded(start:)))
                if (verbose) print *, "Substituted __FILE__ with '", trim(filepath), "', expanded: '", trim(expanded), "'"
            end if
        end do

        ! Substitute __LINE__
        pos = 1
        do while (pos > 0)
            pos = index(expanded, '__LINE__')
            if (pos > 0) then
                if (pos > 0) then
                    start = pos + len('__LINE__')
                    expanded = trim(expanded(:pos - 1)//tostring(iline)//trim(expanded(start:)))
                    if (verbose) print *, "Substituted __LINE__ with '", iline, "', expanded: '", trim(expanded), "'"
                end if
            end if
        end do
    end function

    recursive function expand_macros(line, macros) result(expanded)
        character(*), intent(in)            :: line
        type(macro_t), intent(in)           :: macros(:)
        !private
        character(:), allocatable :: expanded, args_str, temp, va_args, token1, token2, prefix, suffix
        character(MAX_LINE_LEN) :: arg_values(MAX_PARAMS)
        integer :: i, pos, start, paren_level, arg_start, arg_count, c
        integer :: j, macro_start, macro_end, k, token1_start, token2_stop
        integer :: quote1, quote2
        logical :: isopened
        character :: quote

        expanded = line
        if (size(macros) == 0) return
        isopened = .false.
        if (verbose) print *, "Initial expanded: '", trim(expanded), "'"
        do i = 1, size(macros)
            if (len_trim(macros(i)%name) == 0) cycle
            c = 0
            do while (c < len_trim(expanded))
                c = c + 1
                if (expanded(c:c) == '"' .or. expanded(c:c) == "'") then
                    if (.not. isopened) then
                        isopened = .true.
                        quote = expanded(c:c)
                    else    
                        if (expanded(c:c) == quote) isopened = .false.
                    end if
                end if
                if (isopened) cycle
                if (c + len_trim(macros(i)%name) - 1 > len_trim(expanded)) exit
                if (expanded(c:c + len_trim(macros(i)%name) - 1) == trim(macros(i)%name)) then
                    pos = c
                    c = c + len_trim(macros(i)%name) - 1
                    macro_start = pos
                    start = pos + len_trim(macros(i)%name)
                    if (macros(i)%num_params > 0 .or. macros(i)%is_variadic) then
                        if (start <= len(expanded)) then
                            if (expanded(start:start) == '(') then
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
                                if (verbose) print *, "Expanding macro: ", trim(macros(i)%name), ", args: ", trim(args_str)
                                temp = trim(macros(i)%value)

                                if (macros(i)%is_variadic) then
                                    if (arg_count < macros(i)%num_params) then
                                        if (verbose) print *, "Error: Too few arguments for macro ", trim(macros(i)%name)
                                        cycle
                                    end if
                                    va_args = ''
                                    do j = macros(i)%num_params + 1, arg_count
                                        if (j > macros(i)%num_params + 1) va_args = va_args//', '
                                        va_args = va_args//trim(arg_values(j))
                                    end do
                                    if (verbose) print *, "__VA_ARGS__: '", trim(va_args), "'"
                                else if (arg_count /= macros(i)%num_params) then
                                    if (verbose) print *, "Error: Incorrect number of arguments for macro ", trim(macros(i)%name)
                                    cycle
                                end if

                                ! Handle concatenation (##) first with immediate substitution
                                block
                                    pos = 1
                                    do while (pos > 0)
                                        pos = index(temp, '##')
                                        if (pos > 0) then
                                            ! Find token1 (before ##)
                                            k = pos - 1
                                            if (k <= 0) then
                                                if (verbose) print *, "Error: No token before ##"
                                                cycle
                                            end if

                                            token1 = adjustr(temp(:k))
                                            prefix = ''
                                            token1_start = index(token1, ' ')
                                            if (token1_start > 0) then
                                                prefix = token1(:token1_start)
                                                token1 = token1(token1_start + 1:)
                                            end if

                                            ! Substitute token1 and mark as used
                                            do j = 1, macros(i)%num_params
                                                if (trim(token1) == trim(macros(i)%params(j))) then
                                                    token1 = trim(arg_values(j))
                                                    exit
                                                end if
                                            end do

                                            ! Find token2 (after ##)
                                            k = pos + 2
                                            if (k > len(temp)) then
                                                if (verbose) print *, "Error: No token after ##"
                                                cycle
                                            end if

                                            suffix = ''
                                            token2 = adjustl(temp(k:))
                                            token2_stop = index(token2, ' ')
                                            if (token2_stop > 0) then
                                                suffix = token2(token2_stop:)
                                                token2 = token2(:token2_stop - 1)
                                            end if

                                            ! Substitute token2 and mark as used
                                            do j = 1, macros(i)%num_params
                                                if (trim(token2) == trim(macros(i)%params(j))) then
                                                    token2 = trim(arg_values(j))
                                                    exit
                                                end if
                                            end do

                                            ! Concatenate, replacing the full 'token1 ## token2' pattern
                                            temp = trim(prefix//trim(token1)//trim(token2)//suffix)
                                            if (verbose) print *, "Concatenated '", trim(token1), "' and '", trim(token2), &
                                                "' to '", trim(token1)//trim(token2), "', temp: '", trim(temp), "'"
                                        end if
                                    end do
                                end block

                                ! Handle stringification (#param)
                                block
                                    do j = 1, macros(i)%num_params
                                        pos = 1
                                        do while (pos > 0)
                                            pos = index(temp, '#'//trim(macros(i)%params(j)))
                                            if (pos > 0) then
                                                start = pos + 1 + len_trim(macros(i)%params(j))
                                                temp = trim(temp(:pos - 1)//'"'//trim(arg_values(j))//'"'//trim(temp(start:)))
                                                if (verbose) print *, "Stringified param ", j, ": '", trim(macros(i)%params(j)), "' to '", &
                                                    trim(arg_values(j)), "', temp: '", trim(temp), "'"
                                            end if
                                        end do
                                    end do
                                end block

                                ! Substitute regular parameters (only if not used by ## or #)
                                block
                                    integer :: c1
                                    logical :: opened

                                    opened = .false.
                                    do j = 1, macros(i)%num_params
                                        c1 = 0
                                        do while (c1 < len_trim(temp))
                                            c1 = c1 + 1
                                            if (temp(c1:c1) == '"') opened = .not. opened
                                            if (c1 > 1) then
                                                if (temp(c1 - 1:c1 - 1) == '#') exit
                                            end if
                                            if (opened .or. opened) cycle
                                            if (c1 + len_trim(macros(i)%params(j)) - 1 > len_trim(temp)) exit
                                            if (temp(c1:c1 + len_trim(macros(i)%params(j)) - 1) == trim(macros(i)%params(j))) then
                                                if (len_trim(temp) > 1) then
                                                    if (c1 == 1) then
                                                        if (verify(temp(c1 + 1:c1 + 1), ' ()[]<>&;.,!/*-+\="'//"'") /= 0) cycle
                                                    end if
                                                    if (c1 > len_trim(temp)) then
                                                        if (verify(temp(c1 - 1:c1 - 1), ' ()[]<>&;.,!/*-+\="'//"'") /= 0) cycle
                                                    end if
                                                    if (len(temp) > c1) then
                                                        if (verify(temp(c1 - 1:c1 - 1), ' ()[]<>&;.,!/*-+\="'//"'") /= 0 &
                                                            .and. verify(temp(c1 + 1:c1 + 1), ' ()[]<>$&;.,!/*-+\="'//"'") /= 0) cycle
                                                    else
                                                        if (verify(temp(c1 - 1:c1 - 1), ' ()[]<>&;.,!/*-+\="'//"'") /= 0) cycle
                                                    end if
                                                end if
                                                pos = c1
                                                c1 = c1 + len_trim(macros(i)%params(j)) - 1
                                                start = pos + len_trim(macros(i)%params(j))
                                                temp = trim(temp(:pos - 1)//trim(arg_values(j))//trim(temp(start:)))
                                                if (verbose) print *, "Substituted param ", j, ": '", trim(macros(i)%params(j)), "' with '", &
                                                    trim(arg_values(j)), "', temp: '", trim(temp), "'"
                                            end if
                                        end do
                                    end do
                                end block

                                ! Substitute __VA_ARGS__
                                block
                                    if (macros(i)%is_variadic) then
                                        pos = 1
                                        do while (pos > 0)
                                            pos = index(temp, '__VA_ARGS__')
                                            if (pos > 0) then
                                                start = pos + len('__VA_ARGS__')
                                                if (start < len(temp) .and. temp(start:start) == '_' &
                                                    .and. temp(start + 1:start + 1) == ')') then
                                                    temp = trim(temp(:pos - 1)//trim(va_args)//')')
                                                else
                                                    temp = trim(temp(:pos - 1)//trim(va_args)//trim(temp(start:)))
                                                end if
                                                if (verbose) print *, "Substituted __VA_ARGS__ with '", trim(va_args), &
                                                    "', temp: '", trim(temp), "'"
                                            end if
                                        end do
                                    end if
                                end block

                                if (verbose) print *, "Before recursive call, temp: '", trim(temp), "'"
                                temp = expand_macros(temp, macros) ! Only for nested macros
                                if (verbose) print *, "After recursive call, temp: '", trim(temp), "'"
                                if (verbose) print *, "Prefix: '", trim(expanded(:macro_start - 1)), "'"
                                if (verbose) print *, "Temp: '", trim(temp), "'"
                                if (verbose) print *, "Suffix: '", trim(expanded(macro_end + 1:)), "'"
                                expanded = trim(expanded(:macro_start - 1)//trim(temp)//expanded(macro_end + 1:))
                                if (verbose) print *, "After substitution, expanded: '", trim(expanded), "'"
                            end if
                        end if
                    else
                        temp = trim(macros(i)%value)
                        macro_end = start - 1
                        expanded = trim(expanded(:macro_start - 1)//trim(temp)//expanded(macro_end + 1:))
                        expanded = expand_macros(expanded, macros)
                        if (verbose) print *, "Simple macro expanded: '", trim(expanded), "'"
                    end if
                end if
            end do
        end do
    end function

    logical function is_defined(name, macros, idx) result(res)
        character(*), intent(in)    :: name
        type(macro_t), intent(in)   :: macros(:)
        integer, intent(inout), optional :: idx
        !private
        integer :: i
        
        res = .false.
        do i = 1, size(macros)
            if (trim(macros(i)%name) == trim(name)) then
                res = .true.
                if (present(idx)) idx = i
                exit
            end if
        end do
    end function

    function tostring(any)
        class(*), intent(in) :: any
        !private
        character(:), allocatable   :: tostring
        character(4096)             :: line

        call print_any(any); tostring = trim(line)
    contains
        subroutine print_any(any)
            use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64, real32, real64, real128
            class(*), intent(in)     :: any

            select type (any)
            type is (integer(kind=int8)); write (line, '(i0)') any
            type is (integer(kind=int16)); write (line, '(i0)') any
            type is (integer(kind=int32)); write (line, '(i0)') any
            type is (integer(kind=int64)); write (line, '(i0)') any
            type is (real(kind=real32)); write (line, '(1pg0)') any
            type is (real(kind=real64)); write (line, '(1pg0)') any
            type is (real(kind=real128)); write (line, '(1pg0)') any
            type is (logical); write (line, '(1l)') any
            type is (character(len=*)); write (line, '(a)') any
            type is (complex); write (line, '("(",1pg0,",",1pg0,")")') any
            end select
        end subroutine
    end function
end module
