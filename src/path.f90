!> @brief A modern, portable Fortran module for path manipulation and basic directory operations.
!! This module provides a clean interface for working with file system paths
!! in a platform-independent way. It correctly handles both Unix (`/`) and Windows (`\`) path
!! separators through conditional compilation and offers deferred-length character results
!! for maximum flexibility.
!!
!! The module builds upon the `fpx_string` module for `string` type support and provides
!! overloads of key procedures to accept either intrinsic `character(*)` or `type(string)`
!! arguments.
!!
!! Features
!! --------
!! - Detection of absolute and rooted paths on Windows and Unix-like systems
!! - Safe path joining that avoids duplicate separators
!! - Extraction of directory part, filename (with or without extension)
!! - Path splitting into head/tail components (similar to Python's `os.path.split`)
!! - Retrieval of the current working directory (`cwd`)
!! - Changing the current working directory (`chdir`)
!!
!! @note All path-returning functions return allocatable deferred-length characters.
!! @note The public generic `join` interface works with any combination of `character` and `string`.
!!
!! @example
!! ```fortran
!! use fpx_path
!! character(:), allocatable :: p1, p2, full
!!
!! p1 = "/home/user/docs"
!! p2 = "report.pdf"
!! full = join(p1, p2)                ! => "/home/user/docs/report.pdf"
!!
!! print *, is_absolute(full)         ! .true.  (on Unix)
!! print *, filename(full)            ! "report"
!! print *, filename(full,.true.)     ! "report.pdf"
!! print *, dirpath(full)             ! "/home/user/docs"
!! ```
!!
!! @example Windows example
!! ```fortran
!! use fpx_path
!! character(:), allocatable :: p
!! p = join("C:\Users", "Alice", "Documents")
!! ! p == "C:\Users\Alice\Documents"
!! print *, is_absolute(p)   ! .true.
!! ```
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
    
    !> Generic interface for joining two path components
    !! Supports all combinations of character and string arguments
    interface join 
        module procedure :: join_character_character
        module procedure :: join_string_character
        module procedure :: join_character_string
        module procedure :: join_string_string
    end interface
    
    contains
    
    !> @brief Returns .true. if the path is absolute.
    !! On Unix a path is absolute when it starts with '/'.
    !! On Windows a path is absolute when it starts with a drive letter followed by ':\'
    !! (e.g. "C:\", "d:/temp").
    !!
    !! @param[in] filepath  Path to test
    !! @return res          .true. if filepath is absolute
    !!
    !! @example
    !!    print *, is_absolute("/home/user")   ! .true.  (Unix)
    !!    print *, is_absolute("C:\\Temp")     ! .true.  (Windows)
    !!    print *, is_absolute("docs/..")      ! .false.
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
    
    !> @brief Returns .true. if the path is rooted (starts with a separator) or is absolute.
    !! A rooted path begins with the platform separator ("\" on Windows, "/" elsewhere)
    !! even if it is not a full absolute path (e.g. "\temp" on Windows).
    !!
    !! @param[in] filepath  Path to test
    !! @return res          .true. if filepath is rooted
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
    
    !> @brief Extracts the filename part of a path.
    !! By default the extension is stripped. If keepext=.true. the full filename
    !! including extension is returned.
    !!
    !! @param[in] filepath   Full or relative path
    !! @param[in] keepext    Optional; keep extension when .true.
    !! @return res           Filename (without path)
    !!
    !! @example
    !!    print *, filename("dir/file.txt")        ! "file"
    !!    print *, filename("dir/file.txt",.true.) ! "file.txt"
    !!    print *, filename("archive.tar.gz")      ! "archive.tar"
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
    
    !> @brief Joins two path components with the correct platform separator.
    !! Removes duplicate separators and trailing/leading whitespace.
    !!
    !! @param[in] path1  First path component
    !! @param[in] path2  Second path component
    !! @return res       Joined path
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
    
    !> @brief Joins character path with string path component.
    !! Removes duplicate separators and trailing/leading whitespace.
    !!
    !! @param[in] path1  First path component
    !! @param[in] path2  Second path component
    !! @return res       Joined path
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
    
    !> @brief Joins string path with character path component.
    !! Removes duplicate separators and trailing/leading whitespace.
    !!
    !! @param[in] path1  First path component
    !! @param[in] path2  Second path component
    !! @return res       Joined path
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
    
    !> @brief Joins two string path components.
    !! Removes duplicate separators and trailing/leading whitespace.
    !!
    !! @param[in] path1  First path component
    !! @param[in] path2  Second path component
    !! @return res       Joined path
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
    
    !> @brief Returns the directory part of a path (everything before the last separator).
    !! @param[in] filepath  Path to analyse
    !! @return res          Directory component
    !!
    !! @example
    !!    print *, dirpath("/home/user/file.txt")  ! "/home/user"
    pure function dirpath(filepath) result(res)
        character(*), intent(in) :: filepath
        character(:), allocatable :: res
        !private
        character(:), allocatable :: temp

        call split_path(filepath, res, temp)
    end function
    
    !> @brief Returns the base name (filename) part of a path.
    !! @param[in] filepath  Path to analyse
    !! @return res          Base name component
    !!
    !! @example
    !!    print *, dirname("/home/user/file.txt")  ! "file.txt"
    pure function dirname(filepath) result(res)
        character(*), intent(in) :: filepath
        character(:), allocatable :: res
        !private
        character(:), allocatable :: temp

        call split_path(filepath, temp, res)
    end function
    
    !> @brief Splits a path into head (directory) and tail (basename) components.
    !! @param[in]  filepath  Input path
    !! @param[out] head      Directory part (includes trailing separator when appropriate)
    !! @param[out] tail      Base name part
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
    
    !> @brief Returns the current working directory as a deferred-length character string.
    !! Returns empty string on failure.
    !!
    !! @return res  Current working directory
    !!
    !! @example
    !!    character(:), allocatable :: here
    !!    here = cwd()
    !!    print *, "We are in: ", here
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

    !> @brief Changes the current working directory.
    !! @param[in]  path  Directory to change to
    !! @param[out] err   Optional integer error code (0 = success, non-zero = failure)
    !!
    !! @example
    !!    integer :: ierr
    !!    call chdir("/tmp", ierr)
    !!    if (ierr /= 0) stop "Failed to change directory"
    subroutine chdir(path, err)
        character(*) :: path
        integer, optional, intent(out) :: err
        integer :: loc_err

        loc_err =  chdir_c(path//c_null_char)

        if (present(err)) err = loc_err
    end subroutine
end module