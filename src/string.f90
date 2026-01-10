!> @file
!! @defgroup group_string String
!! Minimal yet powerful variable-length string type with modern Fortran features.
!! This module implements a lightweight `string` derived type that behaves like
!! a true variable-length character string while remaining fully compatible with
!! intrinsic Fortran character operations.
!!
!! Features:
!! - Automatic memory management via `allocatable character(:)`
!! - Overloaded assignment (`=`) between `string` and `character(*)`
!! - Overloaded operators: `//` (concatenation), `==` (equality), `.contains.` (membership)
!! - Generic interfaces for `len`, `len_trim`, `trim`
!! - Full support for formatted I/O (`write`, `print`)
!! - Helper routines for parsing Fortran source (line continuation, uppercase conversion, etc.)
!!
!! The design is intentionally minimal — it provides only what's necessary for
!! robust string handling in scientific and preprocessing applications,
!! avoiding the bloat of larger string libraries while remaining fast and standards-compliant.
!! @note All procedures are `pure` or `elemental` when possible for maximum performance
!!       and usability in array contexts.
!!
!! <h2 class="groupheader">Examples</h2>
!! @par Basic Usage
!! @code{.f90}
!! type(string) :: s, t
!! character(:), allocatable :: line
!!
!! s = 'Hello'              ! Assignment from literal
!! t = s // ' World!'       ! Concatenation
!! print *, t%chars         ! Output: Hello World!
!!
!! if (s == 'Hello') then
!!      print *, 'Equal'
!! else
!!      print *, 'Case sensitive'
!! endif
!!
!! print *, len(t)          ! -> 12
!! print *, len_trim(t)     ! -> 12
!! ...
!! @endcode
!!
!! @par Array and Container Support
!! @code{.f90}
!! type(string) :: words(3)
!! logical      :: found
!!
!! words = [string('apple'), string('banana'), string('cherry')]
!! found = words .contains. 'banana'     ! --> .true.
!! found = words .contains. string('date') ! --> .false.
!! ...
!! @endcode
!!
!! @par Advanced: Source Code Processing
!! @code{.f90}
!! character(len=:), allocatable :: code_line
!! code_line = uppercase('program hello_world  ! comment')  ! --> 'PROGRAM HELLO_WORLD  ! comment'
!! ...
!! @endcode
module fpx_string
    use fpx_constants
    implicit none; private
    
    public :: len,          &
              len_trim,     &
              trim,         &
              operator(//), &
              operator(.contains.)
    
    public :: starts_with,  &
              head,         &
              tail,         &
              previous,     &
              concat,       &
              writechk,     &
              uppercase
    
    !> Represents text as a sequence of ASCII code units.
    !!        The derived type wraps an allocatable character array.
    !!
    !! <h2 class="groupheader">Examples</h2>
    !! @code{.f90}
    !! type(string) :: s
    !! s = 'foo'
    !! @endcode
    !!
    !! <h2 class="groupheader">Constructors</h2>
    !! Initializes a new instance of the string class
    !! <h3>string(character(:))</h3>
    !! @verbatim type(string) function string(character(:) chars) @endverbatim
    !! 
    !! @param[in] chars 
    !! 
    !! @b Examples
    !! @code{.f90}
    !! type(string) :: s
    !! s = string('foo')
    !! @endcode
    !! @return The constructed string object.
    !!
    !! <h2 class="groupheader">Remarks</h2>
    !! The string implementation proposed here is kept at the bare 
    !! minimum of what is required by the library. There are many 
    !! other implementations that can be found.
    !!
    !! @ingroup group_string
    type, public :: string
        character(:), allocatable :: chars !< Variable length character array
    contains
        !! @cond
        procedure, pass(lhs), private    :: character_assign_string
        procedure, pass(rhs), private    :: string_assign_character
        procedure, pass(lhs), private    :: string_eq_string !! Equal to string logical operator.
        procedure, pass(lhs), private    :: string_eq_character !! Equal to character logical operator.
        procedure, pass(rhs), private    :: character_eq_string !! Equal to character (inverted) logical operator.
        procedure, pass(dtv), private    :: write_formatted !! Formatted output.
        !! @endcond
        generic, public :: assignment(=) => character_assign_string, &
                                            string_assign_character
        generic, public :: operator(==)  => string_eq_string, &
                                            string_eq_character, &
                                            character_eq_string
        generic, public :: write(formatted) => write_formatted
    end type
    
    !> Return the length of a string
    !!
    !! @b Remarks
    !! @ingroup group_string
    interface len
        module procedure :: string_len
    end interface
    
    !> Return the trimmed length of a string
    !!
    !! @b Remarks
    !! @ingroup group_string
    interface len_trim
        module procedure :: string_len_trim
    end interface
    
    !> Return the trimmed string
    !!
    !! @b Remarks
    !! @ingroup group_string
    interface trim
        module procedure :: string_trim
    end interface
    
    !> Concatenation operator
    !!
    !! @b Remarks
    !! @ingroup group_string
    interface operator(//)
        module procedure :: string_concat_string
        module procedure :: string_concat_character
        module procedure :: character_concat_string
    end interface
    
    !> Check whether a string belongs to a list or not
    !!
    !! @b Remarks
    !! @ingroup group_string
    interface operator(.contains.)
        module procedure :: strings_contain_string
        module procedure :: strings_contain_character
        module procedure :: characters_contain_string
        module procedure :: characters_contain_character
    end interface
    
    contains
    
    !> Assignment overloading. Assign a character array to a string.
    !! @param[inout] lhs string
    !! @param[in]    rhs character(*)
    !! 
    !! @b Examples
    !! @code{.f90}
    !! type(string) :: s
    !! 
    !! s = 'foo'
    !! @endcode
    !!
    !! @b Remarks
    subroutine character_assign_string(lhs, rhs)
        class(string), intent(inout)   :: lhs
        character(*), intent(in)       :: rhs
        
        if (allocated(lhs%chars)) deallocate(lhs%chars)
        allocate(lhs%chars, source = rhs)
    end subroutine
    
    !> Assignment overloading. Assign a string to a character array.
    !! @param[inout] lhs character(:), allocatable
    !! @param[in]    rhs string
    !! 
    !! @b Examples
    !! @code{.f90}
    !! type(string) :: s
    !! character(:), allocatable :: c
    !! 
    !! s = 'foo'
    !! c = s
    !! ! The value of c is now 'foo'
    !! @endcode
    !!
    !! @b Remarks
    subroutine string_assign_character(lhs, rhs)
        character(:), allocatable, intent(inout) :: lhs
        class(string), intent(in)                :: rhs
        
        lhs = rhs%chars
    end subroutine
    
    !> Length of the string entity.
    !! @param[in] this string  
    !! 
    !! @b Examples
    !! @code{.f90}
    !! type(string) :: s
    !! integer :: l
    !! 
    !! s = string('foo ')
    !! l = len(s)
    !! ! The value of l is 4
    !! @endcode
    !! @return An integer corresponding to the length of the string.
    !!
    !! @b Remarks
    elemental integer function string_len(this) result(res)
        class(string), intent(in) :: this
         
        if (allocated(this%chars)) then
            res = len(this%chars)
        else
            res = 0
        end if
    end function
    
    !> Length of the string entity without trailing blanks (len_trim).
    !! @param[in] this string  
    !! 
    !! @b Examples
    !! @code{.f90}
    !! type(string) :: s
    !! integer :: l
    !! 
    !! s = string('foo ')
    !! l = len_trim(s)
    !! ! The value of l is 3
    !! @endcode
    !! @return An integer corresponding to the trimmed length of the string.
    !!
    !! @b Remarks
    pure integer function string_len_trim(this) result(res)
         class(string), intent(in) :: this
         
        if (allocated(this%chars)) then
            res = len_trim(this%chars)
        else
            res = 0
        end if
    end function
    
    !> Returns a copy of the string with trailing blanks removed.
    !! @param[in] this string
    !! @return Trimmed character string (deferred length).
    !!
    !! @b Remarks
    pure function string_trim(this) result(res)
        class(string), intent(in) :: this
        character(:), allocatable :: res
        
        if (allocated(this%chars)) then
            res = trim(this%chars)
        else
            res = ''
        end if
    end function
    
    !> Concatenation of two string objects.
    !! @param[in] lhs left-hand side string
    !! @param[in] rhs right-hand side string
    !! @return New concatenated string.
    !!
    !! @b Remarks
    pure function string_concat_string(lhs, rhs) result(res)
        class(string), intent(in) :: lhs
        class(string), intent(in) :: rhs
        character(:), allocatable :: res
        
        if (allocated(lhs%chars) .and. allocated(rhs%chars)) then
            res = lhs%chars // rhs%chars
        elseif (allocated(lhs%chars)) then
            res = lhs%chars
        elseif (allocated(rhs%chars)) then
            res = rhs%chars
        else
            res = ''
        end if
    end function
    
    !> Concatenation of string and character expression.
    !! @param[in] lhs string
    !! @param[in] rhs character expression
    !! @return New concatenated string.
    !!
    !! @b Remarks
    pure function string_concat_character(lhs, rhs) result(res)
        class(string), intent(in)   :: lhs
        character(*), intent(in)    :: rhs
        character(:), allocatable   :: res
        
        if (allocated(lhs%chars)) then
            res = lhs%chars // rhs
        else
            res = rhs
        end if
    end function
    
    !> Concatenation of character expression and string.
    !! @param[in] lhs character expression
    !! @param[in] rhs string
    !! @return New concatenated string.
    !!
    !! @b Remarks
    pure function character_concat_string(lhs, rhs) result(res)
        character(*), intent(in)    :: lhs
        class(string), intent(in)   :: rhs
        character(:), allocatable   :: res
        
        if (allocated(rhs%chars)) then
            res = lhs // rhs%chars
        else
            res = lhs
        end if
    end function
    
    !> Equality comparison between two string objects.
    !! @param[in] lhs left-hand side
    !! @param[in] rhs right-hand side
    !! @return .true. if the strings are equal, .false. otherwise.
    !!
    !! @b Remarks
    elemental function string_eq_string(lhs, rhs) result(res)
        class(string), intent(in) :: lhs !! Left hand side.
        type(string), intent(in) :: rhs !! Right hand side.
        logical                   :: res !! Opreator test result.

        if (.not. allocated(lhs%chars)) then
            res = allocated(rhs%chars)
        else
            res = lhs%chars == rhs%chars
        end if
    end function

    !> Equality comparison between string and character expression.
    !! @param[in] lhs string
    !! @param[in] rhs character expression
    !! @return .true. if equal, .false. otherwise.
    !!
    !! @b Remarks
    elemental function string_eq_character(lhs, rhs) result(res)
        class(string), intent(in) :: lhs !! Left hand side.
        character(*), intent(in) :: rhs !! Right hand side.
        logical                               :: res !! Opreator test result.

        if (.not. allocated(lhs%chars)) then
            res = .false.
        else
            res = lhs%chars == rhs
        end if
    end function

    !> Equality comparison (reversed) between character expression and string.
    !! @param[in] lhs character expression
    !! @param[in] rhs string
    !! @return .true. if equal, .false. otherwise.
    !!
    !! @b Remarks
    elemental function character_eq_string(lhs, rhs) result(res)
        character(*), intent(in) :: lhs !! Left hand side.
        class(string), intent(in) :: rhs !! Right hand side.
        logical                               :: res !! Operator test result.

        if (.not. allocated(rhs%chars)) then
            res = .false.
        else
            res = rhs%chars == lhs
        end if
    end function
    
    !> Formatted output procedure for user-defined type @ref string (UDTIO)
    !! This procedure is called automatically when a formatted WRITE statement is used  
    !! with a variable of type `string` (when using the DT edit descriptor or default  
    !! formatted output for the type).
    !!
    !! It writes the content of the string component `dtv%chars` using a simple `A` format.  
    !! If the string is not allocated, an empty string is written.
    !!
    !! @param[in] dtv       The @ref string object to be written (polymorphic dummy argument)
    !! @param[in] unit      Fortran logical unit number
    !! @param[in] iotype    String describing the edit descriptor ('DT' + optional string)
    !! @param[in] v_list    Integer array containing the values from the DT edit descriptor  
    !!                      (v_list is empty if no parentheses were used after DT)
    !! @param[out] iostat   I/O status code (0 = success, positive = error, negative = end-of-file/end-of-record)
    !! @param[inout] iomsg  Message describing the I/O error (if any)
    !!
    !! @b Note
    !! - This implementation **ignores** `iotype` and `v_list` parameters  
    !!   → the same simple character output is always performed
    !! - The procedure always uses format `(A)`
    !! - Empty (not allocated) string is written as empty line (zero characters)
    !!
    !! @b Warning
    !! This is a minimal implementation of UDTIO formatted output.  
    !! More sophisticated versions could:
    !! - respect `iotype` (DT"..." or LISTDIRECTED)
    !! - use `v_list` for width/precision control
    !! - add quotation marks, escaping, etc.
    !!
    !! @b Examples
    !! @code{.f90}
    !! type(string) :: s
    !! call s%set("Hello formatted world!")
    !!
    !! write(*, *)     s               ! may call write_formatted (depending on compiler)
    !! write(*, '(DT)') s              ! explicitly calls write_formatted
    !! @endcode
    !!
    !! @b Remarks
    subroutine write_formatted(dtv, unit, iotype, v_list, iostat, iomsg)
        class(string), intent(in)   :: dtv 
        integer, intent(in)         :: unit !! Logical unit.
        character(*), intent(in)    :: iotype !! Edit descriptor.
        integer, intent(in)         :: v_list(:) !! Edit descriptor list.
        integer, intent(out)        :: iostat !! IO status code.
        character(*), intent(inout) :: iomsg !! IO status message.

        if (allocated(dtv%chars)) then
            write (unit, '(A)', iostat=iostat, iomsg=iomsg) dtv%chars
        else
            write (unit, '(A)', iostat=iostat, iomsg=iomsg) ''
        end if
    end subroutine
    
    !> Checks if a string starts with a given prefix
    !! Returns `.true.` if the string `str` (after trimming leading/trailing whitespace)
    !! begins exactly with the substring `arg1`.
    !! The function uses `index()` after trimming both strings with `trim(adjustl())`.
    !!
    !! @param[in] str    The string to be tested
    !! @param[in] arg1   The prefix to look for at the beginning of `str`
    !! @param[out] idx  (optional) If present, receives the starting position of `arg1` in the trimmed string
    !!                       (will be 1 if the function returns `.true.`, otherwise >1 or 0)
    !!
    !! @return `.true.` if `str` starts with `arg1` (after trimming), `.false.` otherwise
    !!
    !! @b Note
    !! - Leading and trailing whitespace of both `str` and `arg1` is ignored
    !! - Comparison is case-sensitive
    !! - Empty `arg1` will always return `.true.` (any string starts with empty string)
    !!
    !! @b Warning
    !! The returned index (when requested) is the position **after trimming** of the input string,
    !! not in the original untrimmed string.
    !!
    !! @b Examples
    !! @code{.f90}
    !! character(80) :: line = '   hello world  '
    !! logical :: ok
    !! integer :: pos
    !!
    !! ok = starts_with(line, 'hello')               ! → .true.
    !! ok = starts_with(line, 'hello', pos)          ! → .true. and pos = 1
    !! ok = starts_with(line, 'world')               ! → .false.
    !! ok = starts_with('  test123  ', 'test')       ! → .true.
    !! ...
    !! @endcode
    !!
    !! @b Remarks
    !! @ingroup group_string
    logical function starts_with(str, arg1, idx) result(res)
        character(*), intent(in) :: str
        character(*), intent(in) :: arg1
        integer, intent(out), optional :: idx
        !private
        integer :: i
        
        i = index(trim(adjustl(str)), trim(arg1))
        res = (i == 1)
        if (present(idx)) idx = i
    end function
    
    !> Returns the first non-blank character of a string.
    !! @param[in] str input string
    !! @return First character (space if empty)
    !!
    !! @b Remarks
    !! @ingroup group_string
    character function head(str) result(res)
        character(*), intent(in) :: str
        
        res = ' '
        if (len_trim(str) == 0) return
        
        res = str(1:1)
    end function
    
    !> Returns the last non-blank character of a string.
    !! @param[in] str input string
    !! @return Last character (space if empty)
    !!
    !! @b Remarks
    !! @ingroup group_string
    character function tail(str) result(res)
        character(*), intent(in) :: str
        !private
        integer :: n
        
        res = ' '; n = len_trim(str)
        if (n == 0) return
        
        res = str(n:n)
    end function
    
    !> Smart concatenation that removes continuation markers (&) and handles line-continuation rules.
    !! @param[in] str1 first line
    !! @param[in] str2 second line
    !! @return Concatenated string with proper continuation handling
    !!
    !! @b Remarks
    !! @ingroup group_string
    function concat(str1, str2) result(res)
        character(*), intent(in) :: str1
        character(*), intent(in) :: str2
        character(:), allocatable :: res
        !private
        integer :: n1, n2
        
        n1 = len(str1); n2 = 1
        if (head(str1) == '!') then
            n2 = 2
            if (tail(str1) == '&') n1 = len_trim(str1) - 1
            if (starts_with(str2, '!dir$') .or. starts_with(str2, '!DIR$') .or. &
                starts_with(str2, '!dec$') .or. starts_with(str2, '!DEC$') .or. &
                starts_with(str2, '!gcc$') .or. starts_with(str2, '!GCC$') .or. &
                starts_with(str2, '!acc$') .or. starts_with(str2, '!ACC$') .or. &
                starts_with(str2, '!$omp') .or. starts_with(str2, '!$OMP')) then
                n2 = 6
            end if
            if (head(adjustl(str2(n2:))) == '&') then
                n2 = index(str2, '&') + 1
            end if
        else
            if (tail(str1) == '&') n1 = len_trim(str1) - 1
            if (head(trim(str2)) == '&') n2 = index(str2, '&') + 1
            if (tail(str1(:n1)) == '(') n1 = index(str1(:n1), '(', back=.true.)
        end if
        
        if (len(str1) > 0 .and. len(str2) >= n2) then
            if (str1(n1:n1) == ' ' .and. str2(n2:n2) == ' ') n2 = n2 + 1
        end if
        res = str1(:n1)//str2(n2:)
    end function
    
    !> Convert string to upper case (respects contents of quotes).
    !! @param[in] str input string
    !! @return Upper-case version of the string
    !!
    !! @b Examples
    !! @code
    !! character(*), parameter :: input = 'test'
    !! character(:), allocatable :: output
    !! output = uppercase(input)
    !! if (output == 'TEST') print*, 'OK'
    !! @endcode
    !!
    !! @b Remarks
    !! @ingroup group_string
    pure function uppercase(str) result(res)
        character(*), intent(in) :: str
        character(len_trim(str)) :: res
        !private
        integer :: ilen, ioffset, iquote, iqc, iav, i

        ilen = len_trim(str)
        ioffset = iachar('A') - iachar('a')
        iquote = 0
        res = str
        do i = 1, ilen
            iav = iachar(str(i:i))
            if (iquote == 0 .and. (iav == 34 .or. iav == 39)) then
                iquote = 1
                iqc = iav
                cycle
            end if
            if (iquote == 1 .and. iav == iqc) then
                iquote = 0
                cycle
            end if
            if (iquote == 1) cycle
            if (iav >= iachar('a') .and. iav <= iachar('z')) then
                res(i:i) = achar(iav + ioffset)
            else
                res(i:i) = str(i:i)
            end if
        end do
    end function
    
    !> Write a long line split into chunks of size CHKSIZE with continuation (&).
    !! @param[in] unit logical unit
    !! @param[in] str  string to write
    !!
    !! @b Remarks
    !! @ingroup group_string
    subroutine writechk(unit, str)
        integer, intent(in)         :: unit
        character(*), intent(in)    :: str
        !private
        integer :: i, n
        
        n = 0
        if (head(str) /= '!') then
            n = floor(len(str) / real(CHKSIZE))
            do i = 1, n
                write (unit, '(A)') str((i-1)*CHKSIZE+1:i*CHKSIZE) // '&'
            end do
        end if
        write (unit, '(A)') str(n*CHKSIZE+1:)
    end subroutine
    
    !> Returns the previous non-blank character before position pos (updates pos).
    !! @param[in]    line input line
    !! @param[inout] pos  current position (moved backward)
    !! @return Previous non-blank character
    !!
    !! @b Remarks
    !! @ingroup group_string
    character(1) function previous(line, pos) result(res)
        character(*), intent(in)    :: line
        integer, intent(inout)      :: pos
        !private

        if (pos == 1) then
            res = trim(line(pos:pos))
        else
            do while (line(pos:pos) == ' ')
                pos = pos - 1
                if (pos == 1) exit
            end do
            res = line(pos:pos)
        end if
    end function    
    
    !> Checks whether an array of string contains a given string.
    !! @param[in] lhs array of string
    !! @param[in] rhs string to search for
    !! @return .true. if rhs is present in lhs
    !!
    !! @b Remarks
    logical function strings_contain_string(lhs, rhs) result(res)
        type(string), intent(in)    :: lhs(:)
        type(string), intent(in)    :: rhs
        !private
        integer :: i
        
        res = .false.
        do i = 1, size(lhs)
            if (lhs(i) == rhs) then
                res = .true.
                exit
            end if
        end do
    end function
    
    !> Checks whether an array of string contains a given character expression.
    !! @param[in] lhs array of string
    !! @param[in] rhs character expression to search for
    !! @return .true. if rhs is present in lhs
    !!
    !! @b Remarks
    logical function strings_contain_character(lhs, rhs) result(res)
        type(string), intent(in)    :: lhs(:)
        character(*), intent(in)    :: rhs
        !private
        integer :: i
        
        res = .false.
        do i = 1, size(lhs)
            if (lhs(i) == rhs) then
                res = .true.
                exit
            end if
        end do
    end function
    
    !> Checks whether an array of character contains a given character expression.
    !! @param[in] lhs array of character
    !! @param[in] rhs character expression to search for
    !! @return .true. if rhs is present in lhs
    !!
    !! @b Remarks
    logical function characters_contain_character(lhs, rhs) result(res)
        character(*), intent(in)    :: lhs(:)
        character(*), intent(in)    :: rhs
        !private
        integer :: i
        
        res = .false.
        do i = 1, size(lhs)
            if (lhs(i) == rhs) then
                res = .true.
                exit
            end if
        end do
    end function
    
    !> Checks whether an array of character contains a given string.
    !! @param[in] lhs array of character
    !! @param[in] rhs string to search for
    !! @return .true. if rhs is present in lhs
    !!
    !! @b Remarks
    logical function characters_contain_string(lhs, rhs) result(res)
        character(*), intent(in)    :: lhs(:)
        type(string), intent(in)    :: rhs
        !private
        integer :: i
        
        res = .false.
        do i = 1, size(lhs)
            if (lhs(i) == rhs) then
                res = .true.
                exit
            end if
        end do
    end function
end module