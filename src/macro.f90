module fpx_macro
    use fpx_constants
    use fpx_logging
    use fpx_path
    use fpx_graph
    use fpx_string

    implicit none; private

    public :: macro, &
			  add, &
			  get, &
              insert, &
              clear, &
              remove, &
              sizeof
    
    public :: expand_macros, &
              expand_all, &
              is_defined
    
    integer, parameter :: BUFFER_SIZE = 256

    type, extends(string) :: macro
        character(:), allocatable :: value
        type(string), allocatable :: params(:)
        logical :: is_variadic ! New flag for variadic macros
    contains
        private
    end type
    
    interface macro
        module procedure :: macro_new
    end interface
        
    interface add 
        module procedure :: add_item
        module procedure :: add_item_from_name
        module procedure :: add_item_from_name_and_value
        module procedure :: add_range
    end interface
   
    interface clear
        module procedure  :: clear_item
    end interface

    interface get
        module procedure  :: get_item
    end interface

    interface insert
        module procedure :: insert_item
    end interface

    interface remove
        module procedure :: remove_item
    end interface
    
    interface sizeof
        module procedure  :: size_item
    end interface

    contains
    
    type(macro) function macro_new(name, val) result(that)
        character(*), intent(in)            :: name
        character(*), intent(in), optional  :: val
        
        that = trim(name)
        if (present(val)) then
            that%value = val
        else 
            that%value = ''
        end if
        allocate(that%params(0))
        that%is_variadic = .false.
    end function
    
    subroutine set_default(this, name)
        class(macro), intent(inout)   :: this
        character(*), intent(in)        :: name
        
        this = trim(name)
        this%value = ''
    end subroutine
    
    subroutine set_with_value(this, name, value)
        class(macro), intent(inout)   :: this
        character(*), intent(in)        :: name
        character(*), intent(in)        :: value
        
        this = trim(name)
        this%value = value
    end subroutine
    
    function expand_all(line, macros, filepath, iline) result(expanded)
        character(*), intent(in)            :: line
        type(macro), intent(in)           :: macros(:)
        character(*), intent(in)            :: filepath
        integer, intent(in)                 :: iline
        character(:), allocatable :: expanded
        !private
        integer :: pos, start, sep, dot

        if (.not. is_circular(macros)) then
            expanded = expand_macros(line, macros)
        else
            expanded = line
        end if
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
        character(*), intent(in)    :: line
        type(macro), intent(in)     :: macros(:)
        !private
        character(:), allocatable :: expanded, args_str, temp, va_args, token1, token2, prefix, suffix
        type(string) :: arg_values(MAX_PARAMS)
        integer :: c, i, j, k, n, pos, start, paren_level, arg_start, nargs
        integer :: m_start, m_end, token1_start, token2_stop
        logical :: isopened, found
        character :: quote
        integer, allocatable :: indexes(:)

        expanded = line
        if (size(macros) == 0) return
        isopened = .false.
        if (verbose) print *, "Initial expanded: '", trim(expanded), "'"
        
        do i = 1, size(macros)
            n = len_trim(macros(i))
            if (n == 0) cycle
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
                if (c + n - 1 > len_trim(expanded)) exit
                
                found = .false.
                if (expanded(c:c + n - 1) == macros(i)) then
                    found = .true.
                    if (len_trim(expanded(c:)) > n) then
                        found = verify(expanded(c + n:c + n), ' ()[]<>&;.,^~!/*-+\="'//"'") == 0
                    end if
                end if
                
                if (found) then
                    pos = c
                    c = c + n - 1
                    m_start = pos
                    start = pos + n
                    if (size(macros(i)%params) > 0 .or. macros(i)%is_variadic) then
                        if (start <= len(expanded)) then
                            if (expanded(start:start) == '(') then
                                paren_level = 1
                                arg_start = start + 1
                                nargs = 0
                                j = arg_start
                                do while (j <= len(expanded) .and. paren_level > 0)
                                    if (expanded(j:j) == '(') paren_level = paren_level + 1
                                    if (expanded(j:j) == ')') paren_level = paren_level - 1
                                    if (paren_level == 1 .and. expanded(j:j) == ',' .or. paren_level == 0) then
                                        if (nargs < MAX_PARAMS) then
                                            nargs = nargs + 1
                                            arg_values(nargs) = trim(adjustl(expanded(arg_start:j - 1)))
                                            arg_start = j + 1
                                        end if
                                    end if
                                    j = j + 1
                                end do
                                m_end = j - 1
                                args_str = expanded(start:m_end)
                                if (verbose) print *, "Expanding macro: ", macros(i), ", args: ", trim(args_str)
                                temp = trim(macros(i)%value)

                                if (macros(i)%is_variadic) then
                                    if (nargs < size(macros(i)%params)) then
                                        if (verbose) print *, "Error: Too few arguments for macro ", macros(i)
                                        cycle
                                    end if
                                    va_args = ''
                                    do j = size(macros(i)%params) + 1, nargs
                                        if (j > size(macros(i)%params) + 1) va_args = va_args//', '
                                        va_args = va_args//arg_values(j)
                                    end do
                                    if (verbose) print *, "__VA_ARGS__: '", trim(va_args), "'"
                                else if (nargs /= size(macros(i)%params)) then
                                    if (verbose) print *, "Error: Incorrect number of arguments for macro ", macros(i)
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
                                            do j = 1, size(macros(i)%params)
                                                if (trim(token1) == trim(macros(i)%params(j))) then
                                                    token1 = arg_values(j)
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
                                            do j = 1, size(macros(i)%params)
                                                if (trim(token2) == trim(macros(i)%params(j))) then
                                                    token2 = arg_values(j)
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
                                    do j = 1, size(macros(i)%params)
                                        pos = 1
                                        do while (pos > 0)
                                            pos = index(temp, '#'//trim(macros(i)%params(j)))
                                            if (pos > 0) then
                                                start = pos + 1 + len_trim(macros(i)%params(j))
                                                temp = trim(temp(:pos - 1)//'"'//arg_values(j)//'"'//trim(temp(start:)))
                                                if (verbose) print *, "Stringified param ", j, ": '", macros(i)%params(j), "' to '", &
                                                    arg_values(j), "', temp: '", trim(temp), "'"
                                            end if
                                        end do
                                    end do
                                end block

                                ! Substitute regular parameters (only if not used by ## or #)
                                block
                                    integer :: c1
                                    logical :: opened

                                    opened = .false.
                                    do j = 1, size(macros(i)%params)
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
                                                temp = trim(temp(:pos - 1)//arg_values(j)//trim(temp(start:)))
                                                if (verbose) print *, "Substituted param ", j, ": '", macros(i)%params(j), "' with '", &
                                                    arg_values(j), "', temp: '", trim(temp), "'"
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
                                if (verbose) print *, "Prefix: '", trim(expanded(:m_start - 1)), "'"
                                if (verbose) print *, "Temp: '", trim(temp), "'"
                                if (verbose) print *, "Suffix: '", trim(expanded(m_end + 1:)), "'"
                                expanded = trim(expanded(:m_start - 1)//trim(temp)//expanded(m_end + 1:))
                                if (verbose) print *, "After substitution, expanded: '", trim(expanded), "'"
                            end if
                        end if
                    else
                        temp = trim(macros(i)%value)
                        m_end = start - 1
                        expanded = trim(expanded(:m_start - 1)//trim(temp)//expanded(m_end + 1:))
                        expanded = expand_macros(expanded, macros)
                        if (verbose) print *, "Simple macro expanded: '", trim(expanded), "'"
                    end if
                end if
            end do
        end do
    end function
    
    logical function is_circular(macros) result(res)
        type(macro), intent(in) :: macros(:)
        !private
        character(:), allocatable :: expanded
        integer :: i, j, pos, start, paren_level, arg_start, c
        integer :: m_start
        logical :: isopened
        character :: quote
        integer, allocatable :: indexes(:)
        type(digraph) :: graph

        res = .false.
        if (size(macros) == 0) return
        isopened = .false.

        graph = digraph(size(macros))
        
        do j = 1, size(macros)
            expanded = macros(j)%value
            do i = 1, size(macros)
                if (len_trim(macros(i)) == 0) cycle
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
                    if (c + len_trim(macros(i)) - 1 > len_trim(expanded)) exit
                    if (expanded(c:c + len_trim(macros(i)) - 1) == macros(i)) then
                        expanded(c:c + len_trim(macros(i)) - 1) = ' '
                        call graph%add_edge(j, i)
                    end if
                end do
            end do
        end do
        
        res = graph%is_circular(3)
    end function

    logical function is_defined(name, macros, idx) result(res)
        character(*), intent(in)    :: name
        type(macro), intent(in)   :: macros(:)
        integer, intent(inout), optional :: idx
        !private
        integer :: i
        
        res = .false.
        do i = 1, size(macros)
            if (macros(i) == trim(name)) then
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
            type is (character(*)); write (line, '(a)') any
            type is (complex); write (line, '("(",1pg0,",",1pg0,")")') any
            end select
        end subroutine
    end function
    
    subroutine add_to(vec, val, n, chunk_size, finished)
        type(macro), allocatable, intent(inout)    :: vec(:)
        type(macro), intent(in)                    :: val
        integer, intent(inout)                       :: n
        integer, intent(in)                          :: chunk_size
        logical, intent(in)                          :: finished
        !private
        type(macro), allocatable :: tmp(:)
        integer :: csize
       
        csize = chunk_size
        
        if (finished) csize = 1 
        if (allocated(vec)) then
            if (n == size(vec)) then
                ! have to add another chunk:
                allocate (tmp(size(vec) + csize))
                tmp(1:size(vec)) = vec
                call move_alloc(tmp, vec)
            end if
            n = n + 1
        else
            ! the first element:
            allocate (vec(csize))
            n = 1
        end if

        vec(n) = val
        if (finished) then
            if (allocated(tmp)) deallocate (tmp)
            if (n /= size(vec)) then
                allocate (tmp(n), source = vec(1:n))
                call move_alloc(tmp, vec)
            end if
        end if
    end subroutine
    
    subroutine add_item(this, arg)
        type(macro), intent(inout), allocatable    :: this(:)
        type(macro), intent(in)                    :: arg
        !private 
        integer :: count
       
        count = size(this)
        call add_to(this, arg, count, BUFFER_SIZE, finished = .true.)
    end subroutine
    
    subroutine add_item_from_name(this, name)
        type(macro), intent(inout), allocatable   :: this(:)
        character(*), intent(in)                    :: name
        !private 
        integer :: count
        if (.not. allocated(this)) allocate(this(0))
        count = size(this)
        call add_to(this, macro(name), count, BUFFER_SIZE, finished = .true.)
    end subroutine
    
    subroutine add_item_from_name_and_value(this, name, value)
        type(macro), intent(inout), allocatable   :: this(:)
        character(*), intent(in)                    :: name
        character(*), intent(in)                    :: value
        !private 
        integer :: count
        
        if (.not. allocated(this)) allocate(this(0))
        count = size(this)
        call add_to(this, macro(name, value), count, BUFFER_SIZE, finished = .true.)
    end subroutine
    
    subroutine add_range(this, args)
        type(macro), intent(inout), allocatable   :: this(:)
        type(macro), intent(in)                   :: args(:)
        !private 
        integer :: i, n, count
       
        if (.not. allocated(this)) allocate(this(0))
        n = size(args); count = size(this)
        do i = 1, n
            call add_to(this, args(i), count, BUFFER_SIZE, finished = i == n)
        end do
    end subroutine
    
    subroutine clear_item(this)
        class(macro), intent(inout), allocatable :: this(:)
       
        if (allocated(this)) deallocate(this)
        allocate(this(0))
    end subroutine

    function get_item(this, key) result(res)
        type(macro), intent(inout)    :: this(:)
        integer, intent(in)             :: key
        type(macro), allocatable      :: res
        !private
        integer :: n
        
        n = sizeof(this)
        if (key > 0 .and. key <= n) then
            res = this(key)
        end if
    end function
    
    subroutine insert_item(this, i, arg)
        type(macro), intent(inout), allocatable   :: this(:)
        integer, intent(in)                         :: i
        type(macro), intent(in)                   :: arg
        !private 
        integer :: j, count
       
        if (.not. allocated(this)) allocate(this(0))
	    count = size(this)
        call add_to(this, arg, count, BUFFER_SIZE, finished = .true.)
       
        do j = count, i + 1, -1
            this(j) = this(j-1)
        end do
        this(i) = arg
    end subroutine

    integer function size_item(this) result(res)
        type(macro), intent(inout), allocatable  :: this(:)
        
        if (.not. allocated(this)) then 
            res = 0
        else
            res = size(this)
        end if
    end function
    
    subroutine remove_item(this, i)
        type(macro), intent(inout), allocatable   :: this(:)
        integer, intent(in)                         :: i
        !private
        type(macro), allocatable :: tmp(:)
        integer :: j, n

        if (.not. allocated(this)) allocate(this(0))
        n = size(this)
        do j = i, n - 1
            this(j) = this(j + 1)
        end do
        
        allocate (tmp(n - 1), source = this(1:n - 1))
        call move_alloc(tmp, this)
        deallocate(tmp)
    end subroutine
end module
