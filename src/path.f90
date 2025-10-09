module fpx_path
    use, intrinsic :: iso_c_binding
    use fpx_string
    
    implicit none
    
    public
    
#ifdef _WIN32   
    character, parameter    :: separator = '\'
    character(*), parameter :: alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
#else
    character, parameter :: separator = '/'
#endif

    interface
        function getcwd_c(buf, size) &
#ifdef _WIN32
            bind(C,name='_getcwd') result(r)
#else
            bind(C,name='getcwd') result(r)
#endif
        import
            type(c_ptr) :: r
            character(kind=c_char), intent(out) :: buf(*)
            integer(kind=c_size_t), value :: size
        end function
    end interface
    
    interface
        integer(c_int) function chdir_c(path) bind(C, name='chdir')
        import
            character(kind=c_char) :: path(*)
        end function
    end interface
    
    interface join 
        module procedure :: join_character_character
        module procedure :: join_string_character
        module procedure :: join_character_string
        module procedure :: join_string_string
    end interface
    
    contains
    
    pure logical function is_absolute(filepath) result(res)
        character(*), intent(in)        :: filepath
#ifdef _WIN32   
        if (len(filepath) < 2) then
            res = .false.
            return
        end if
        res = scan(filepath(1:1), alphabet) /= 0 .and. filepath(2:2) == ':'
#else
        res = filepath(1:1) == separator
#endif

    end function
    
    pure logical function is_rooted(filepath) result(res)
        character(*), intent(in)        :: filepath
        !private 
        integer :: length
        
        length = len(filepath)
#ifdef _WIN32  
        res = (length >= 1 .and. filepath(1:1) == separator) .or. is_absolute(filepath)
#else
        res = (length > 0 .and. filepath(1:1) == separator)
#endif
    end function
    
    pure function filename(filepath, keepext) result(res)
        character(*), intent(in)        :: filepath
        character(:), allocatable       :: res
        logical, intent(in), optional   :: keepext
        !private
        integer :: ipoint, islash

        ipoint = index(filepath, '.', back=.true.)
        islash = index(filepath, separator, back=.true.)
        if (ipoint < islash) ipoint = len_trim(filepath) + 1
        if (present(keepext)) then
            if (keepext) then
                res = filepath(islash+1:len_trim(filepath))
            else
                res = filepath(islash+1: ipoint-1)
            end if
        else
            res = filepath(islash+1: ipoint-1)
        end if
    end function
    
    pure function join_character_character(path1, path2) result(res)
        character(*), intent(in) :: path1
        character(*), intent(in) :: path2
        character(:), allocatable :: res
        !private
        character(:), allocatable :: temp
        
        temp = trim(adjustl(path1))
        if (temp(len(temp):len(temp))==separator) temp = trim(temp(:len(temp)-1))
        
        res = temp // separator // trim(adjustl(path2))
    end function
    
    pure function join_character_string(path1, path2) result(res)
        character(*), intent(in) :: path1
        type(string), intent(in) :: path2
        character(:), allocatable :: res
        !private
        character(:), allocatable :: temp
        
        temp = trim(adjustl(path1))
        if (temp(len(temp):len(temp))==separator) temp = trim(temp(:len(temp)-1))
        
        res = temp // separator // trim(adjustl(path2%chars))
    end function
    
    pure function join_string_character(path1, path2) result(res)
        type(string), intent(in) :: path1
        character(*), intent(in) :: path2
        character(:), allocatable :: res
        !private
        character(:), allocatable :: temp
        
        temp = trim(adjustl(path1%chars))
        if (temp(len(temp):len(temp))==separator) temp = trim(temp(:len(temp)-1))
        
        res = temp // separator // trim(adjustl(path2))
    end function
    
    pure function join_string_string(path1, path2) result(res)
        type(string), intent(in) :: path1
        type(string), intent(in) :: path2
        character(:), allocatable :: res
        !private
        character(:), allocatable :: temp
        
        temp = trim(adjustl(path1%chars))
        if (temp(len(temp):len(temp))==separator) temp = trim(temp(:len(temp)-1))
        
        res = temp // separator // trim(adjustl(path2%chars))
    end function
    
    pure function dirpath(filepath) result(res)
        character(*), intent(in) :: filepath
        character(:), allocatable :: res
        !private
        character(:), allocatable :: temp

        call split_path(filepath, res, temp)
    end function
    
    pure function dirname(filepath) result(res)
        character(*), intent(in) :: filepath
        character(:), allocatable :: res
        !private
        character(:), allocatable :: temp

        call split_path(filepath, temp, res)
    end function
    
    pure subroutine split_path(filepath, head, tail)
        character(*), intent(in)                :: filepath
        character(:), allocatable, intent(out)  :: head
        character(:), allocatable, intent(out)  :: tail
        !private
        character(:), allocatable :: temp
        integer :: i, ipoint, isep

        ! Empty string, return (.,'')
        if (len_trim(filepath) == 0) then
            head = '.'; tail = ''
            return
        end if

        ! Remove trailing path separators
        temp = trim(adjustl(filepath))
        if (temp(len(temp):len(temp)) == separator) then
            temp = trim(temp(:len(temp)-1))
        else
            ipoint = index(filepath, '.', back=.true.)
            isep = index(filepath, separator, back=.true.)
            if (ipoint > isep .and. isep > 0) then
                temp = trim(temp(:isep-1))
            end if
        end if

        if (len_trim(temp) == 0) then
            head = separator
            tail = ''
            return
        end if

        i = len(temp) - index(temp, separator, back=.true.) + 1

        ! if no `pathsep`, then it probably was a root dir like `C:\`
        if (i == 0) then
            head = temp // separator
            tail = ''
            return
        end if

        head = temp(:len(temp)-i)

        ! child of a root directory
        if (index(temp, separator, back=.true.) == 0) then
            head = head // separator
        end if

        tail = temp(len(temp)-i+2:)
    end subroutine
    
    function cwd() result(res)
        character(:), allocatable :: res
        !private
        character(len=1, kind=c_char) :: buf(256)
        integer :: i, n
        integer(c_size_t) :: s
        
        s = size(buf, kind=c_size_t)
        if (c_associated(getcwd_c(buf, s))) then
            n = findloc(buf, achar(0), 1)
            allocate(character(n-1) :: res)
            do i = 1, n - 1
                res(i:i) = buf(i)
            end do
        else 
            res = ''
        end if
    end function

    subroutine chdir(path, err)
        character(*) :: path
        integer, optional, intent(out) :: err
        integer :: loc_err

        loc_err =  chdir_c(path//c_null_char)

        if (present(err)) err = loc_err
    end subroutine
end module