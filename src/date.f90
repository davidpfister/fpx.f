!> @file
!! @defgroup group_date Date
!! Lightweight, high-performance date/time handling for the fpx preprocessor
!! This module provides a compact `datetime` type and essential operations
!! used primarily for expanding the standard predefined macros:
!! - `__DATE__` → e.g. 'Aug-12-2025'
!! - `__TIME__` → e.g. '14:35:27'
!! - `__TIMESTAMP__` → e.g. 'Tue 12-Aug-2025 14:35:27'
!!
!! Features:
!! - `now()` returns current local date/time using `date_and_time()`
!! - Flexible string formatting via `to_string(fmt)`
!! - Parsing from common string formats (ISO, US, RFC-like)
!! - Day-of-week calculation via Zeller’s congruence
!! - Elemental and pure functions where possible for performance
!! - Minimal memory footprint using small integer kinds (`int8`, `int16`)
!!
!! Used internally by `fpx_macro` during `__DATE__`, `__TIME__`, and `__TIMESTAMP__` expansion.
!! <h2  class="groupheader">Examples</h2>
!!
!! 1. Expand standard predefined macros (as done internally):
!! @code{.f90}
!!    type(datetime) :: dt
!!    dt = now()
!!    print *, '__DATE__      >> ', dt%to_string('MMM-dd-yyyy')      ! __DATE__      >> 'Aug-12-2025'
!!    print *, '__TIME__      >> ', dt%to_string('HH:mm:ss')          ! '__TIME__      >> 14:35:27'
!!    print *, '__TIMESTAMP__ >> ', dt%to_string('ddd-MMM-yyyy HH:mm:ss') ! '__TIMESTAMP__ >> Tue 12-Aug-2025 14:35:27'
!!    ...
!! @endcode
!!
!! 2. Parse date from string:
!! @code{.f90}
!!    type(datetime) :: build_time
!!    build_time = datetime('2025-08-12 09:30:00')
!!    print *, 'build on: ', build_time%to_string('ddd-MMM-yyyy')
!!    ...
!! @endcode
!!
!! 3. Get current time for logging:
!! @code{.f90}
!!    type(datetime) :: dt
!!    dt = now()
!!    print *, 'Preprocessing started at ', dt%to_string('HH:mm:ss')
!!    ...
!! @endcode
module fpx_date
    use, intrinsic :: iso_fortran_env, only: i1 => int8, &
            i2 => int16
    implicit none; private

    public :: now

    !> Compact representation of date and time
    !! Stores all components in minimal integer kinds to reduce memory usage.
    !! All fields are public for easy access.
    !! <h2  class="groupheader">Examples</h2>
    !! @code{.f90}
    !!    type(datetime) :: bt
    !!    bt = datetime('2025-08-12 09:30:00')
    !!    print *, 'build on: ', bt%to_string('ddd-MMM-yyyy')
    !!    ...
    !! @endcode
    !! <h2  class="groupheader">Remarks</h2>
    !! The date implementation proposed here is kept at the bare
    !! minimum of what is required by the library. There are many
    !! other implementations that can be found.
    !! <h2  class="groupheader">Constructors</h2>
    !! Initializes a new instance of the @ref datetime class
    !! <h3>datetime(character(*),  character(*))</h3>
    !! @verbatim type(datetime) function datetime(character(*) string, (optional) character(*) fmt) @endverbatim
    !!
    !! @param[in] string date as string
    !! @param[in] fmt (optional) date format
    !!
    !! @b Examples
    !! @code{.f90}
    !! type(datetime) :: d
    !! d = datetime('2025-08-12 09:30:00')
    !! @endcode
    !! <h3>datetime(integer, integer, integer, integer, integer, integer, integer)</h3>
    !! @verbatim type(datetime) function datetime((optional) integer year, (optional) integer month, ...) @endverbatim
    !!
    !! @param[in] year (optional)
    !! @param[in] month (optional)
    !! @param[in] day (optional)
    !! @param[in] hour (optional)
    !! @param[in] minute (optional)
    !! @param[in] second (optional)
    !! @param[in] millisecond (optional)
    !!
    !! @b Examples
    !! @code{.f90}
    !! type(datetime) :: d
    !! d = datetime(1970, 1, 1)
    !! @endcode
    !! @return The constructed datetime object.
    !!
    !! <h2  class="groupheader">Remarks</h2>
    !! @ingroup group_date
    type, public :: datetime
        private
        integer(i2), public  :: year    !< Year
        integer(i1), public  :: month   !< Month
        integer(i1), public  :: day     !< Day
        integer(i1), public  :: hour    !< Hour
        integer(i1), public  :: minute  !< Minute
        integer(i1), public  :: second  !< Second
        integer(i2), public  :: millisecond  !< Millisecond
    contains
        procedure, pass(this), public :: to_string => datetime_to_string
        procedure, pass(this), public :: parse => datetime_parse
    end type

    !> Constructor interface for @ref datetime type
    !!
    !! @b Remarks
    !! @ingroup group_date
    interface datetime
        !! @cond
        module procedure :: datetime_new, datetime_new_from_string
        !! @endcond
    end interface

contains

    !> Constructor
    elemental function datetime_new(year, month, day, hour, minute, second, millisecond) result(that)
        integer, intent(in), optional   :: year
        integer, intent(in), optional   :: month
        integer, intent(in), optional   :: day
        integer, intent(in), optional   :: hour
        integer, intent(in), optional   :: minute
        integer, intent(in), optional   :: second
        integer, intent(in), optional   :: millisecond
        type(datetime)                  :: that

        that%year = 0_i2; if (present(year)) that%year = int(year, kind=i2)
        that%month = 0_i1; if (present(month)) that%month = int(month, kind=i1)
        that%day = 0_i1; if (present(day)) that%day = int(day, kind=i1)
        that%hour = 0_i1; if (present(hour)) that%hour = int(hour, kind=i1)
        that%minute = 0_i1; if (present(minute)) that%minute = int(minute, kind=i1)
        that%second = 0_i1; if (present(second)) that%second = int(second, kind=i1)
        that%millisecond = 0_i2; if (present(millisecond)) that%millisecond = int(millisecond, kind=i2)
    end function

    elemental function datetime_new_from_string(string, fmt) result(that)
        character(*), intent(in)            :: string
        character(*), intent(in), optional  :: fmt
        type(datetime)                      :: that

        if (present(fmt)) then
            call that%parse(string, fmt)
        else
            call that%parse(string)
        end if
    end function

    !> Return current local date and time
    !! Uses intrinsic `date_and_time()` and populates all fields including milliseconds.
    !! @return the datetime object corresponding to the current time
    !!
    !! @b Remarks
    !! @ingroup group_date
    function now() result(res)
        type(datetime)  :: res
        !private
        integer :: values(9)

        call date_and_time(values=values)

        res%year = int(values(1), kind=i2)
        res%month = int(values(2), kind=i1)
        res%day = int(values(3), kind=i1)
        res%hour = int(values(5), kind=i1)
        res%minute = int(values(6), kind=i1)
        res%second = int(values(7), kind=i1)
        res%millisecond = int(values(8), kind=i2)
    end function

    !> Returns the day of the week calculated using Zeller's congruence.
    !! Returned value is an integer scalar in the range [0-6], such that:
    !! - 0: Sunday
    !! - 1: Monday
    !! - 2: Tuesday
    !! - 3: Wednesday
    !! - 4: Thursday
    !! - 5: Friday
    !! - 6: Saturday
    !!
    !! @b Remarks
    !! @ingroup group_date
    pure elemental integer function weekday(this)
        class(datetime), intent(in) :: this
        !private
        integer :: year, month, j, k

        year = this%year
        month = this%month

        if (month <= 2) then
            month = month + 12
            year = year - 1
        end if

        j = year / 100
        k = mod(year, 100)

        weekday = mod(this%day + ((month + 1) * 26) / 10 + k + k / 4 + j / 4 + 5 * j, 7) - 1

        if (weekday < 0) weekday = 6
    end function

    !> Parse date/time from string using common formats
    !!
    !! Supports ISO, US, and abbreviated month formats.
    !! On error, defaults to Unix epoch (1970-01-01 00:00:00)
    !! Perform conversion to ISO string
    !! - d: Represents the day of the month as a number from 1 through 31.
    !! - dd: Represents the day of the month as a number from 01 through 31.
    !! - ddd: Represents the abbreviated name of the day (Mon, Tues, Wed, etc).
    !! - dddd: Represents the full name of the day (Monday, Tuesday, etc).
    !! - h: 12-hour clock hour (e.g. 4).
    !! - hh: 12-hour clock, with a leading 0 (e.g. 06)
    !! - H: 24-hour clock hour (e.g. 15)
    !! - HH: 24-hour clock hour, with a leading 0 (e.g. 22)
    !! - m: Minutes
    !! - mm: Minutes with a leading zero
    !! - M: Month number(eg.3)
    !! - MM: Month number with leading zero(eg.04)
    !! - MMM: Abbreviated Month Name (e.g. Dec)
    !! - MMMM: Full month name (e.g. December)
    !! - s: Seconds
    !! - ss: Seconds with leading zero
    !! - t: Abbreviated AM / PM (e.g. A or P)
    !! - tt: AM / PM (e.g. AM or PM
    !! - y: Year, no leading zero (e.g. 2015 would be 15)
    !! - yy: Year, leading zero (e.g. 2015 would be 015)
    !! - yyy: Year, (e.g. 2015)
    !! - yyyy: Year, (e.g. 2015)
    !!
    !! @b Remarks
    !! @ingroup group_date
    elemental subroutine datetime_parse(this, string, fmt)
        class(datetime), intent(inout)      :: this
        character(*), intent(in)            :: string
        character(*), intent(in), optional  :: fmt
        !private
        integer :: ierr
        logical :: valid
        character(256) :: errmsg
        character(len(string)) :: tmp
        character(:), allocatable :: dftfmt

        if (present(fmt)) then
            dftfmt = fmt
        else
            if (len_trim(string) == 10) then
                dftfmt = 'yyyy-MM-dd'
            else
                dftfmt = 'yyyy-MM-dd HH:mm:ss'
            end if
        end if

        tmp = string

        this%year = 0_i2; this%month = 0_i1; this%day = 0_i1
        this%hour = 0_i1; this%minute = 0_i1; this%second = 0_i1; this%millisecond = 0_i2

        select case (dftfmt)
        case ('MMM-dd-yyyy')
            select case (tmp(:3))
            case ('Jan'); tmp(:3) = ' 01'
            case ('Feb'); tmp(:3) = ' 02'
            case ('Mar'); tmp(:3) = ' 03'
            case ('Apr'); tmp(:3) = ' 04'
            case ('May'); tmp(:3) = ' 05'
            case ('Jun'); tmp(:3) = ' 06'
            case ('Jul'); tmp(:3) = ' 07'
            case ('Aug'); tmp(:3) = ' 08'
            case ('Sep'); tmp(:3) = ' 09'
            case ('Oct'); tmp(:3) = ' 10'
            case ('Nov'); tmp(:3) = ' 11'
            case ('Dec'); tmp(:3) = ' 12'
            end select
            read(tmp(2:), '(i2.2,1x,i2.2,1x,i4.4)', iostat=ierr, iomsg=errmsg) &
                    this%month, &
                    this%day, &
                    this%year
        case ('MMM-dd-yyyy HH:mm:ss', 'MMM-dd-yyyyTHH:mm:ss')
            select case (tmp(:3))
            case ('Jan'); tmp(:3) = ' 01'
            case ('Feb'); tmp(:3) = ' 02'
            case ('Mar'); tmp(:3) = ' 03'
            case ('Apr'); tmp(:3) = ' 04'
            case ('May'); tmp(:3) = ' 05'
            case ('Jun'); tmp(:3) = ' 06'
            case ('Jul'); tmp(:3) = ' 07'
            case ('Aug'); tmp(:3) = ' 08'
            case ('Sep'); tmp(:3) = ' 09'
            case ('Oct'); tmp(:3) = ' 10'
            case ('Nov'); tmp(:3) = ' 11'
            case ('Dec'); tmp(:3) = ' 12'
            end select
            read(tmp(2:), '(i2.2,1x,i2.2,1x,i4.4,1x,i2.2,2(1x,i2.2))', iostat=ierr, iomsg=errmsg) &
                    this%month, &
                    this%day, &
                    this%year, &
                    this%hour, &
                    this%minute, &
                    this%second
        case ('yyyy-MM')
            read(tmp, '(i4.4,1x,i2.2)', iostat=ierr, iomsg=errmsg) &
                    this%year, &
                    this%month
        case ('yyyy-MM-dd')
            read(tmp, '(i4.4,2(1x,i2.2))', iostat=ierr, iomsg=errmsg) &
                    this%year, &
                    this%month, &
                    this%day
        case ('dd-MM-yyyy')
            read(tmp, '(i2.2,1x,i2.2,1x, i4.4)', iostat=ierr, iomsg=errmsg) &
                    this%day, &
                    this%month, &
                    this%year
        case ('MM-dd-yyyy')
            read(tmp, '(i2.2,1x,i2.2,1x,i4.4)', iostat=ierr, iomsg=errmsg) &
                    this%month, &
                    this%day, &
                    this%year
        case ('yyyy-MM-ddTHH:mm:ss', 'yyyy-MM-dd HH:mm:ss')
            read(tmp, '(i4.4,2(1x,i2.2),1x,i2.2,2(1x,i2.2))', iostat=ierr, iomsg=errmsg) &
                    this%year, &
                    this%month, &
                    this%day, &
                    this%hour, &
                    this%minute, &
                    this%second
        case ('HH:mm:ss')
            read(tmp, '(i2.2,2(1x,i2.2))', iostat=ierr, iomsg=errmsg) &
                    this%hour, &
                    this%minute, &
                    this%second
        end select

        if (ierr > 0) then
            this%year = 1970_i2; this%month = 1_i1; this%day = 1_i1
            this%hour = 0_i1; this%minute = 0_i1; this%second = 0_i1; this%millisecond = 0_i2
        end if
    end subroutine

    !> Format datetime as string using flexible format codes
    !! Supports many common patterns including those required for `__DATE__` and `__TIMESTAMP__`.
    !! Default format: 'yyyy-MM-ddTHH:mm:ss'
    !!
    !! @b Remarks
    !! @ingroup group_date
    function datetime_to_string(this, fmt) result(res)
        class(datetime), intent(in)          :: this
        character(*), intent(in), optional  :: fmt
        character(:), allocatable           :: res
        !private
        character   :: sep, dash
        character(:), allocatable :: dftfmt, tmp, tmp2
        integer :: ierr
        character(256) :: errmsg

        if (present(fmt)) then
            dftfmt = fmt
        else
            dftfmt = 'yyyy-MM-ddTHH:mm:ss'
        end if
        ! Manager optional parameters
        sep = merge('T', ' ', index(dftfmt, 'T') > 0)
        dash = merge('-', ' ', index(dftfmt, '-') > 0)

        allocate(character(25) :: res)
        ! Perform conversion to ISO string

        select case (this%month)
        case (1); tmp = 'Jan'
        case (2); tmp = 'Feb'
        case (3); tmp = 'Mar'
        case (4); tmp = 'Apr'
        case (5); tmp = 'May'
        case (6); tmp = 'Jun'
        case (7); tmp = 'Jul'
        case (8); tmp = 'Aug'
        case (9); tmp = 'Sep'
        case (10); tmp = 'Oct'
        case (11); tmp = 'Nov'
        case (12); tmp = 'Dec'
        end select
        select case (weekday(this))
        case (0); tmp2 = 'Sun'
        case (1); tmp2 = 'Mon'
        case (2); tmp2 = 'Tue'
        case (3); tmp2 = 'Wed'
        case (4); tmp2 = 'Thu'
        case (5); tmp2 = 'Fri'
        case (6); tmp2 = 'Sat'
        end select

        select case (dftfmt)
        case ('MMM-dd-yyyy', 'MMM dd yyyy')
            write(res, '(a3,a1,i2.2,a1,i4.4)', iostat=ierr, iomsg=errmsg) &
                    tmp, &
                    dash, &
                    this%day, &
                    dash, &
                    this%year
        case ('MMM-ddd-yyyy', 'MMM ddd yyyy')
            write(res, '(a3,a1,a3," ",i2.2,a1,i4.4)', iostat=ierr, iomsg=errmsg) &
                    tmp, &
                    dash, &
                    tmp2, &
                    this%day, &
                    dash, &
                    this%year
        case ('MMM-dd-yyyy HH:mm:ss', 'MMM-dd-yyyyTHH:mm:ss', 'MMM dd yyyy HH:mm:ss', 'MMM dd yyyyTHH:mm:ss')
            write(res, '(a3,a1,i2.2,a1,i4.4,a1,i2.2,2(":",i2.2))', iostat=ierr, iomsg=errmsg) &
                    tmp, &
                    dash, &
                    this%day, &
                    dash, &
                    this%year, &
                    this%hour, &
                    this%minute, &
                    this%second
        case ('MMM-ddd-yyyy HH:mm:ss', 'MMM-ddd-yyyyTHH:mm:ss', 'MMM ddd yyyy HH:mm:ss', 'MMM ddd yyyyTHH:mm:ss')
            write(res, '(a3,a1,a3," ",i2.2,a1,i4.4,a1,i2.2,2(":",i2.2))', iostat=ierr, iomsg=errmsg) &
                    tmp, &
                    dash, &
                    tmp2, &
                    this%day, &
                    dash, &
                    this%year, &
                    this%hour, &
                    this%minute, &
                    this%second
        case ('yyyy-MM', 'yyyy MM')
            write(res, '(i4.4,a1,i2.2)', iostat=ierr, iomsg=errmsg) &
                    this%year, &
                    dash, &
                    this%month
        case ('yyyy-MM-dd', 'yyyy MM dd')
            write(res, '(i4.4,2(a1,i2.2))', iostat=ierr, iomsg=errmsg) &
                    this%year, &
                    dash, &
                    this%month, &
                    dash, &
                    this%day
        case ('yyyy-MM-ddd', 'yyyy MM ddd')
            write(res, '(i4.4,a1,i2.2,a1,a3," ",i2.2)', iostat=ierr, iomsg=errmsg) &
                    this%year, &
                    dash, &
                    this%month, &
                    dash, &
                    tmp2, &
                    this%day
        case ('dd-MM-yyyy', 'dd MM yyyy')
            write(res, '(i2.2,a1,i2.2,a1,i4.4)', iostat=ierr, iomsg=errmsg) &
                    this%day, &
                    dash, &
                    this%month, &
                    dash, &
                    this%year
        case ('ddd-MM-yyyy', 'ddd MM yyyy')
            write(res, '(a3,a1,i2.2," ",i2.2,a1,i4.4)', iostat=ierr, iomsg=errmsg) &
                    tmp2, &
                    dash, &
                    this%month, &
                    this%day, &
                    dash, &
                    this%year
        case ('MM-dd-yyyy', 'MM dd yyyy')
            write(res, '(i2.2,a1,i2.2,a1,i4.4)', iostat=ierr, iomsg=errmsg) &
                    this%month, &
                    dash, &
                    this%day, &
                    dash, &
                    this%year
        case ('MM-ddd-yyyy', 'MM ddd yyyy')
            write(res, '(i2.2,a1,a3," ",i2.2,a1,i4.4)', iostat=ierr, iomsg=errmsg) &
                    this%month, &
                    dash, &
                    tmp2, &
                    this%day, &
                    dash, &
                    this%year
        case ('yyyy-MM-ddTHH:mm:ss', 'yyyy-MM-dd HH:mm:ss', 'yyyy MM ddTHH:mm:ss', 'yyyy MM dd HH:mm:ss')
            write(res, '(i4.4,2(a1,i2.2),a1,i2.2,2(":",i2.2))', iostat=ierr, iomsg=errmsg) &
                    this%year, &
                    dash, &
                    this%month, &
                    dash, &
                    this%day, &
                    sep, &
                    this%hour, &
                    this%minute, &
                    this%second
        case ('yyyy-MM-dddTHH:mm:ss', 'yyyy-MM-ddd HH:mm:ss', 'yyyy MM dddTHH:mm:ss', 'yyyy MM ddd HH:mm:ss')
            write(res, '(i4.4,a1,i2.2,a1,a3," ",i2.2,a1,i2.2,2(":",i2.2))', iostat=ierr, iomsg=errmsg) &
                    this%year, &
                    dash, &
                    this%month, &
                    dash, &
                    tmp2, &
                    this%day, &
                    sep, &
                    this%hour, &
                    this%minute, &
                    this%second
        case ('HH:mm:ss')
            write(res, '(i2.2,2(":",i2.2))', iostat=ierr, iomsg=errmsg) &
                    this%hour, &
                    this%minute, &
                    this%second
        end select
        res = trim(res)
    end function
end module
