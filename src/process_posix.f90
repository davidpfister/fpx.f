!> @file
!! @defgroup group_process_posix POSIX process utilities
!! Low-level POSIX implementation of process inspection routines.
!!
!! This module provides helper procedures used internally by
!! `fpx_process` to inspect running processes on Unix-like systems.
!!
!! Detection strategy:
!!
!! 1. `/proc/<pid>/comm` (Linux only, fastest)
!! 2. `ps`
!!
!! `/proc` is preferred because it requires no subprocess creation and
!! returns only the executable name.
!!
!! The `ps` fallback works on Linux, macOS, FreeBSD, OpenBSD,
!! Solaris and virtually every POSIX implementation.
!!
!! @ingroup group_process
module fpx_process_posix
    use, intrinsic :: iso_fortran_env

    implicit none; private

    public :: get_process_name_posix, &
            get_parent_pid_posix

contains

    !> @brief Return parent PID (PPID) on POSIX systems
    !!
    !! Minimal, deterministic implementation based on /proc/self/status.
    !! No shell, no parsing hacks, no external dependencies.
    !! @ingroup group_process_posix
    integer function get_parent_pid_posix() result(ppid)
        !private
        integer :: u, ios
        character(256) :: line

        ppid = -1

        open(newunit=u, file='/proc/self/status', status='old', action='read', iostat=ios)
        if (ios /= 0) return
        do
            read(u, '(A)', iostat=ios) line
            if (ios /= 0) exit

            ! PPid line format:
            ! PPid:\t1234
            if (line(1:5) == 'PPid:') then
                read(line(6:), *) ppid
                exit
            end if
        end do

        close(u)
    end function

    !> Return executable name of a process.
    !!
    !! Attempts Linux `/proc/<pid>/comm` first and falls back
    !! to invoking `ps`.
    !!
    !! @param[in] pid process identifier
    !!
    !! @return executable name or empty string
    !!
    !! @ingroup group_process
    function get_process_name_posix(pid) result(name)
        integer(int64), intent(in) :: pid
        character(:), allocatable  :: name

        name = process_name_proc(pid)

        if (len(name) == 0) then
            name = process_name_ps(pid)
        endif
    end function

    !> Linux implementation using `/proc/<pid>/comm`
    !!
    !! Returns an empty string if `/proc` is unavailable.
    !!
    !! @ingroup group_process
    function process_name_proc(pid) result(name)
        integer(int64), intent(in) :: pid
        character(:), allocatable :: name
        !private
        character(64)  :: filename
        character(256) :: line
        integer :: ios, unit

        write(filename,'("/proc/",I0,"/comm")') pid

        open(newunit = unit, file = trim(filename), status = 'old', action  = 'read', iostat = ios)

        if (ios /= 0) then
            name = ''
            return
        endif

        read(unit,'(A)',iostat=ios) line
        close(unit)

        if (ios /= 0) then
            name = ''
        else
            name = trim(line)
        endif
    end function

    !> Portable POSIX fallback using `ps`.
    !!
    !! Works on Linux, macOS, FreeBSD, OpenBSD and Solaris.
    !!
    !! @ingroup group_process
    function process_name_ps(pid) result(name)
        integer(int64), intent(in) :: pid
        character(:), allocatable :: name
        !private
        character(64)  :: command, tmpfile
        character(256) :: line
        integer :: ios, unit

        tmpfile = '__fpx_process.tmp'

        write(command,'("ps -p ",I0," -o comm= > ",A)') pid, trim(tmpfile)

        call execute_command_line(command)

        open(newunit = unit, file = trim(tmpfile), status = 'old', action = 'read', iostat = ios)
        if (ios /= 0) then
            name = ''
            close(unit, status='delete')
            return
        endif

        read(unit,'(A)', iostat=ios) line
        if (ios == 0) then
            name = trim(adjustl(line))
        else
            name = ''
        endif

        close(unit, status='delete')
    end function

end module