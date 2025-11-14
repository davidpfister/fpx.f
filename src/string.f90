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
              concat,       &
              writechk,     &
              uppercase
        
    !> @class string
    !! @ingroup group_string
    !! @brief Represents text as a sequence of ASCII code units.
    !!          The derived type wraps an allocatable character array.
    !! <h2>Examples</h2>
    !! ```fortran
    !! type(string) :: s
    !! s = 'foo'
    !! ```
    !! <h2>Remarks</h2>
    !! @par
    !! The string implementation proposed here is kept at the bare 
    !! minimum of what is required by the library. There are many 
    !! other implementations that can be found.
    !! <h2>Constructors</h2>
    !! Initializes a new instance of the @ref string class
    !! <h3>string(character(:))</h3>
    !! @verbatim type(string) function string(character(:) chars) @endverbatim
    !! 
    !! @param[in] chars 
    !! 
    !! @b Examples
    !! ```fortran
    !! type(string) :: s
    !! s = string('foo')
    !! ```
    type, public :: string
        character(:), allocatable :: chars !< Variable length character array
    contains
        procedure, pass(lhs), private    :: character_assign_string
        procedure, pass(rhs), private    :: string_assign_character
        generic, public :: assignment(=) => character_assign_string, &
                                            string_assign_character
        procedure, private, pass(lhs)    :: string_eq_string !! Equal to string logical operator.
        procedure, private, pass(lhs)    :: string_eq_character !! Equal to character logical operator.
        procedure, private, pass(rhs)    :: character_eq_string !! Equal to character (inverted) logical operator.
        generic :: operator(==)          => string_eq_string, &
                                            string_eq_character, &
                                            character_eq_string !! Equal operator overloading.
         procedure, private, pass(dtv)   :: write_formatted !! Formatted output.
         generic :: write (formatted)    => write_formatted !! Formatted output.
    end type
    
    interface len
        module procedure :: string_len
    end interface
    
    interface len_trim
        module procedure :: string_len_trim
    end interface
    
    interface trim
        module procedure :: string_trim
    end interface
    
    interface operator(//)
        module procedure :: string_concat_string
        module procedure :: string_concat_character
        module procedure :: character_concat_string
    end interface
    
    interface operator(.contains.)
        module procedure :: strings_contain_string
        module procedure :: strings_contain_character
        module procedure :: characters_contain_string
        module procedure :: characters_contain_character
    end interface
    
    contains
    
    !> @brief Assignment overloading. Assign a character array
    !!          to a string.
    !! @param[inout] lhs string
    !! @param[in] rhs character(*)
    !! 
    !! @b Examples
    !! ```fortran
    !! type(string) :: s
    !! 
    !! s = 'foo'
    !! ```
    !! @b Remarks
    subroutine character_assign_string(lhs, rhs)
        class(string), intent(inout)   :: lhs
        character(*), intent(in)       :: rhs
        
        if (allocated(lhs%chars)) deallocate(lhs%chars)
        allocate(lhs%chars, source = rhs)
    end subroutine
    
    !> @brief Assignment overloading. Assign a string to a 
    !!          character array
    !! @param[inout] lhs character(*), allocatable
    !! @param[in] rhs string
    !! 
    !! @b Examples
    !! ```fortran
    !! type(string) :: s
    !! character(:), allocatable :: c
    !! 
    !! s = 'foo'
    !! c = s
    !! ! The value of c is now 'foo'
    !! ```
    !! @b Remarks
    subroutine string_assign_character(lhs, rhs)
        character(:), allocatable, intent(inout) :: lhs
        class(string), intent(in)                :: rhs
        
        lhs = rhs%chars
    end subroutine
    
    !> @ingroup group_string
    !> @brief Length of the string entity
    !! @param[in] this string  
    !! 
    !! @b Examples
    !! ```fortran
    !! type(string) :: s
    !! integer :: l
    !! 
    !! s = string('foo ')
    !! l = len(s)
    !! ! The value of l is 4
    !! ```
    !! @returns An integer cooresponding to the length of the string
    elemental integer function string_len(this) result(res)
        class(string), intent(in) :: this
         
        if (allocated(this%chars)) then
            res = len(this%chars)
        else
            res = 0
        end if
    end function
    
    !> @ingroup group_string
    !> @brief Length of the string entity
    !! @param[in] this string  
    !! 
    !! @b Examples
    !! ```fortran
    !! type(string) :: s
    !! integer :: l
    !! 
    !! s = string('foo ')
    !! l = len(s)
    !! ! The value of l is 3
    !! ```
    !! @returns An integer corresponding to the length of the string
    pure integer function string_len_trim(this) result(res)
         class(string), intent(in) :: this
         
        if (allocated(this%chars)) then
            res = len_trim(this%chars)
        else
            res = 0
        end if
    end function
    
    pure function string_trim(this) result(res)
        class(string), intent(in) :: this
        character(:), allocatable :: res
        
        if (allocated(this%chars)) then
            res = trim(this%chars)
        else
            res = ''
        end if
    end function
    
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
    
    character function head(str) result(res)
        character(*), intent(in) :: str
        
        res = ' '
        if (len_trim(str) == 0) return
        
        res = str(1:1)
    end function
    
    character function tail(str) result(res)
        character(*), intent(in) :: str
        !private
        integer :: n
        
        res = ' '; n = len_trim(str)
        if (n == 0) return
        
        res = str(n:n)
    end function
    
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
    
    !> @brief   Convert string to upper case
    !! @ingroup group_string
    !! param[in] str input string
    !!
    !! @returns _character(*)_. A string with uppercase characters.
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