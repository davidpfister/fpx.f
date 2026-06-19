!> @file
!! @defgroup group_os OS
!! This module provides portable runtime operating-system detection
!! facilities used throughout the fpx preprocessor.
!!
!! Supported platforms include:
!! - Linux distributions
!! - macOS
!! - Native Microsoft Windows
!! - Cygwin
!! - Solaris/OpenSolaris
!! - FreeBSD
!! - OpenBSD
!!
!! Detection is performed lazily on first use and cached using
!! OpenMP threadprivate storage, ensuring negligible overhead for
!! repeated queries.
!!
!! The implementation relies primarily on environment variables,
!! with fallback detection through the presence of well-known
!! operating-system specific files.
!!
!! This strategy is designed to work reliably in native
!! installations, containers, WSL environments, and most
!! cross-compilation setups.
!!
!! @par Detection Model
!! OS identification is attempted in the following order:
!! 1. Environment variable `OSTYPE`
!! 2. Environment variable `OS`
!! 3. Operating-system specific filesystem probes
!! 4. Fallback to OS_UNKNOWN
!!
!! @section os_examples Examples
!!
!! 1. Basic OS detection:
!! @code{.f90}
!!    integer :: my_os
!!    my_os = get_os_type()
!!    print *, 'Running on: ', os_name(my_os)
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
!!
!! 4. Module constants
!!
!! @code{.f90}
!! if (get_os_type() == OS_WINDOWS) then
!!     ...
!! end if
!! ...
!! @endcode
module fpx_os
    implicit none; private

    public ::   get_os_type, &
            os_is_unix, &
            os_name

    !> @brief Unknown / undetected operating system
    !! @ingroup group_os
    integer, parameter, public :: OS_UNKNOWN = 0
    !> @brief Linux (any distribution, including GNU/Linux)
    !! @ingroup group_os
    integer, parameter, public :: OS_LINUX = 1
    !> @brief macOS (Darwin-based Apple operating system)
    !! @ingroup group_os
    integer, parameter, public :: OS_MACOS = 2
    !> @brief Microsoft Windows (native, 32-bit or 64-bit)
    !! @ingroup group_os
    integer, parameter, public :: OS_WINDOWS = 3
    !> @brief Cygwin POSIX environment on Windows
    !! @ingroup group_os
    integer, parameter, public :: OS_CYGWIN = 4
    !> @brief Oracle Solaris / OpenSolaris derivatives
    !! @ingroup group_os
    integer, parameter, public :: OS_SOLARIS = 5
    !> @brief FreeBSD and its direct derivatives
    !! @ingroup group_os
    integer, parameter, public :: OS_FREEBSD = 6
    !> @brief OpenBSD
    !! @ingroup group_os
    integer, parameter, public :: OS_OPENBSD = 7
    !> @brief Native Microsoft Windows running on 32-bit x86 architecture.
    !!
    !! This value is returned when the operating system is identified
    !! as Windows and the PROCESSOR_ARCHITECTURE environment variable
    !! indicates an x86 target.
    !!
    !! It can be used when architecture-specific behavior is required.
    !!
    !! @ingroup group_os
    integer, parameter, public :: OS_WINDOWSx86 = 8

contains

    !> Return a human-readable string describing the OS type flag
    !! Converts any of the OS_* integer constants into its corresponding name.
    !! Accepted values include:
    !! - OS_UNKNOWN
    !! - OS_LINUX
    !! - OS_MACOS
    !! - OS_WINDOWS
    !! - OS_WINDOWSx86
    !! - OS_CYGWIN
    !! - OS_SOLARIS
    !! - OS_FREEBSD
    !! - OS_OPENBSD
    !! Useful for logging, error messages, or user output.
    !! @param[in] os OS identifier from get_os_type()
    !! @return    Allocated character string with the OS name
    !!
    !! @b Examples
    !!
    !! @code{.f90}
    !! print *, os_name(OS_LINUX)
    !! !> prints: Linux
    !! ...
    !! @endcode
    !!
    !! @ingroup group_os
    pure function os_name(os) result(res)
        integer, intent(in) :: os
        character(:), allocatable :: res

        select case (os)
        case (OS_LINUX);   res = 'Linux'
        case (OS_MACOS);   res = 'macOS'
        case (OS_WINDOWS); res = 'Windows'
        case (OS_CYGWIN);  res = 'Cygwin'
        case (OS_SOLARIS); res = 'Solaris'
        case (OS_FREEBSD); res = 'FreeBSD'
        case (OS_OPENBSD); res = 'OpenBSD'
        case (OS_UNKNOWN); res = 'Unknown'
        case default     ; res = 'UNKNOWN'
        end select
    end function

    !> Determine the current operating system type
    !! Returns one of the OS_* constants. 
    !!
    !! @par Thread Safety
    !! The detected value is cached independently for each OpenMP thread
    !! using threadprivate storage. Concurrent calls therefore incur no
    !! synchronization overhead after the first query on each thread.
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
    !!
    !! @b Examples
    !!
    !! @code{.f90}
    !! select case (get_os_type())
    !! case (OS_WINDOWS)
    !!     print *, 'Windows'
    !! case (OS_LINUX)
    !!     print *, 'Linux'
    !! end select
    !! ...
    !! @endcode
    !!
    !! @see OS_NAME
    !! @see os_is_unix
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
        inquire(file='/etc/os-release', exist=file_exists)

        if (file_exists) then
            r = OS_LINUX
            ret = r
            return
        end if

        ! macOS
        inquire(file='/usr/bin/sw_vers', exist=file_exists)

        if (file_exists) then
            r = OS_MACOS
            ret = r
            return
        end if

        ! FreeBSD
        inquire(file='/bin/freebsd-version', exist=file_exists)

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
    !! @b Examples
    !!
    !! @code{.f90}
    !! if (os_is_unix()) then
    !!     call execute_command_line('uname -a')
    !! end if
    !! ...
    !! @endcode
    !!
    !! @see get_os_type
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
