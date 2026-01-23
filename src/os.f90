!> @file
!! @defgroup group_os OS
!! Operating system detection utilities for the fpx preprocessor
!! This lightweight module provides reliable runtime detection of the current operating
!! system on Unix-like platforms (Linux, macOS, FreeBSD, OpenBSD, Solaris) and Windows
!! (including Cygwin, MSYS, and native Windows). Detection is performed only once per
!! thread (using OpenMP threadprivate storage) and then cached for fast subsequent calls.
!! The implementation first checks common environment variables (`OSTYPE`, `OS`),
!! then falls back to the presence of OS-specific files. This makes it robust across
!! native systems, containers, WSL, Cygwin, and cross-compilation environments.
!!
!! <h2 class="groupheader">Examples</h2>
!!
!! 1. Basic OS detection:
!! @code{.f90}
!!    integer :: my_os
!!    my_os = get_os_type()
!!    print *, 'Running on: ', OS_NAME(my_os)
!!    !> prints e.g. 'Running on: Linux'
!! @endcode
!!
!! 2. Conditional compilation based on OS:
!! @code{.f90}
!!    !platform specific system call
!!    if (os_is_unix()) then
!!        call system('gcc --version')
!!    else
!!        call execute_command_line('gfortran --version')
!!    end if
!!    ...
!! @endcode
!!
!! 3. Using the cached value explicitly:
!! @code{.f90}
!!    integer :: os_type
!!    os_type = get_os_type()           ! detects and caches
!!    print *, os_is_unix(os_type)      ! fast, no re-detection
!!    ...
!! @endcode
module fpx_os
    implicit none; private

    public ::   get_os_type, &
                os_is_unix

                        public :: OS_NAME
    !> @brief Unknown / undetected operating system
    !! @ingroup group_os
    integer, parameter, public :: OS_UNKNOWN = 0
    !> @brief Linux (any distribution, including GNU/Linux)
    !! @ingroup group_os
    integer, parameter, public :: OS_LINUX   = 1
    !> @brief macOS (Darwin-based Apple operating system)
    !! @ingroup group_os
    integer, parameter, public :: OS_MACOS   = 2
    !> @brief Microsoft Windows (native, 32-bit or 64-bit)
    !! @ingroup group_os
    integer, parameter, public :: OS_WINDOWS = 3
    !> @brief Cygwin POSIX environment on Windows
    !! @ingroup group_os
    integer, parameter, public :: OS_CYGWIN  = 4
    !> @brief Oracle Solaris / OpenSolaris derivatives
    !! @ingroup group_os
    integer, parameter, public :: OS_SOLARIS = 5
    !> @brief FreeBSD and its direct derivatives
    !! @ingroup group_os
    integer, parameter, public :: OS_FREEBSD = 6
    !> @brief OpenBSD
    !! @ingroup group_os
    integer, parameter, public :: OS_OPENBSD = 7
    !> @brief Microsoft Windows — explicitly 32-bit (x86) architecture.
    !!
    !! Mainly useful when different behavior is needed between 32-bit and 64-bit Windows
    !! @ingroup group_os
    integer, parameter, public :: OS_WINDOWSx86 = 8

contains

    !> Return a human-readable string describing the OS type flag
    !! Converts any of the OS_* integer constants into its corresponding name.
    !! Useful for logging, error messages, or user output.
    !! @param[in] os OS identifier from get_os_type()
    !! @return    Allocated character string with the OS name
    !!
    !! @b Remarks
    !! @ingroup group_os
    pure function OS_NAME(os)
        integer, intent(in) :: os
        character(:), allocatable :: OS_NAME

        select case (os)
            case (OS_LINUX);   OS_NAME =  'Linux'
            case (OS_MACOS);   OS_NAME =  'macOS'
            case (OS_WINDOWS); OS_NAME =  'Windows'
            case (OS_CYGWIN);  OS_NAME =  'Cygwin'
            case (OS_SOLARIS); OS_NAME =  'Solaris'
            case (OS_FREEBSD); OS_NAME =  'FreeBSD'
            case (OS_OPENBSD); OS_NAME =  'OpenBSD'
            case (OS_UNKNOWN); OS_NAME =  'Unknown'
            case default     ; OS_NAME =  'UNKNOWN'
        end select
    end function

    !> Determine the current operating system type
    !! Returns one of the OS_* constants. Detection is performed only on the first call
    !! and cached in threadprivate storage for subsequent fast access.
    !!
    !! Detection strategy:
    !! 1. Environment variable `OSTYPE` (common on Unix-like systems)
    !! 2. Environment variable `OS` (set on Windows)
    !! 3. Presence of OS-specific files (/etc/os-release, /usr/bin/sw_vers, etc.)
    !!
    !! Returns OS_UNKNOWN if no reliable indicator is found.
    !!
    !! @return OS identifier (OS_LINUX, OS_MACOS, OS_WINDOWS, ...)
    !!
    !! @b Remarks
    !! @ingroup group_os
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

    !> Return .true. if the current (or supplied) OS is Unix-like
    !! Convenience wrapper that returns .true. for any non-Windows platform.
    !! Useful for writing portable code that needs different handling on Windows.
    !! @param[in] os Optional OS identifier; if absent get_os_type() is called
    !! @return   .true. if OS is not Windows, .false. otherwise
    !!
    !! @b Remarks
    !! @ingroup group_os
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