!> This module contains procedures that interact with the programming environment.
!!
!! * [get_os_type] -- Determine the OS type
!! * [get_env] -- return the value of an environment variable
module fpx_os
    implicit none; private
    
    public ::   get_os_type, &
                os_is_unix
    
                        public :: OS_NAME
    integer, parameter, public :: OS_UNKNOWN = 0
    integer, parameter, public :: OS_LINUX   = 1
    integer, parameter, public :: OS_MACOS   = 2
    integer, parameter, public :: OS_WINDOWS = 3
    integer, parameter, public :: OS_CYGWIN  = 4
    integer, parameter, public :: OS_SOLARIS = 5
    integer, parameter, public :: OS_FREEBSD = 6
    integer, parameter, public :: OS_OPENBSD = 7
    integer, parameter, public :: OS_WINDOWSx86 = 8
    
contains

    !> Return string describing the OS type flag
    pure function OS_NAME(os)
        integer, intent(in) :: os
        character(:), allocatable :: OS_NAME

        select case (os)
            case (OS_LINUX);   OS_NAME =  "Linux"
            case (OS_MACOS);   OS_NAME =  "macOS"
            case (OS_WINDOWS); OS_NAME =  "Windows"
            case (OS_CYGWIN);  OS_NAME =  "Cygwin"
            case (OS_SOLARIS); OS_NAME =  "Solaris"
            case (OS_FREEBSD); OS_NAME =  "FreeBSD"
            case (OS_OPENBSD); OS_NAME =  "OpenBSD"
            case (OS_UNKNOWN); OS_NAME =  "Unknown"
            case default     ; OS_NAME =  "UNKNOWN"
        end select
    end function

    !> Determine the OS type
    !! Returns one of OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_WINDOWS, OS_CYGWIN,
    !! OS_SOLARIS, OS_FREEBSD, OS_OPENBSD.
    !!
    !! At first, the environment variable `OS` is checked, which is usually
    !! found on Windows. Then, `OSTYPE` is read in and compared with common
    !! names. If this fails too, check the existence of files that can be
    !! found on specific system types only.
    !!
    !! Returns OS_UNKNOWN if the operating system cannot be determined.
    integer function get_os_type() result(r)
        character(len=255) :: val
        integer            :: length, rc
        logical            :: file_exists
        logical, save      :: first_run = .true.
        integer, save      :: ret = OS_UNKNOWN
        !$omp threadprivate(ret, first_run)

        if (.not. first_run) then
            r = ret
            return
        end if

        first_run = .false.
        r = OS_UNKNOWN

        ! Check environment variable `OSTYPE`.
        call get_environment_variable('OSTYPE', val, length, rc)

        if (rc == 0 .and. length > 0) then
            ! Linux
            if (index(val, 'linux') > 0) then
                r = OS_LINUX
                ret = r
                return
            end if

            ! macOS
            if (index(val, 'darwin') > 0) then
                r = OS_MACOS
                ret = r
                return
            end if

            ! Windows, MSYS, MinGW, Git Bash
            if (index(val, 'win') > 0 .or. index(val, 'msys') > 0) then
                r = OS_WINDOWS
                ret = r
                return
            end if

            ! Cygwin
            if (index(val, 'cygwin') > 0) then
                r = OS_CYGWIN
                ret = r
                return
            end if

            ! Solaris, OpenIndiana, ...
            if (index(val, 'SunOS') > 0 .or. index(val, 'solaris') > 0) then
                r = OS_SOLARIS
                ret = r
                return
            end if

            ! FreeBSD
            if (index(val, 'FreeBSD') > 0 .or. index(val, 'freebsd') > 0) then
                r = OS_FREEBSD
                ret = r
                return
            end if

            ! OpenBSD
            if (index(val, 'OpenBSD') > 0 .or. index(val, 'openbsd') > 0) then
                r = OS_OPENBSD
                ret = r
                return
            end if
        end if

        ! Check environment variable `OS`.
        call get_environment_variable('OS', val, length, rc)

        if (rc == 0 .and. length > 0 .and. index(val, 'Windows_NT') > 0) then
            r = OS_WINDOWS
            ret = r
            call get_environment_variable('PROCESSOR_ARCHITECTURE', val, length, rc)
            if (rc == 0 .and. length > 0 .and. index(val, 'x86') > 0) then
                r = OS_WINDOWSx86
                ret = r
            end if
            return
        end if

        ! Linux
        inquire (file='/etc/os-release', exist=file_exists)

        if (file_exists) then
            r = OS_LINUX
            ret = r
            return
        end if

        ! macOS
        inquire (file='/usr/bin/sw_vers', exist=file_exists)

        if (file_exists) then
            r = OS_MACOS
            ret = r
            return
        end if

        ! FreeBSD
        inquire (file='/bin/freebsd-version', exist=file_exists)

        if (file_exists) then
            r = OS_FREEBSD
            ret = r
            return
        end if
    end function

    !> Compare the output of [[get_os_type]] or the optional
    !! passed INTEGER value to the value for OS_WINDOWS
    !! and return .TRUE. if they match and .FALSE. otherwise
    logical function os_is_unix(os)
        integer, intent(in), optional :: os
        integer :: build_os
        if (present(os)) then
            build_os = os
        else
            build_os = get_os_type()
        end if
        os_is_unix = build_os /= OS_WINDOWS
    end function
end module