!> @file
!! @defgroup group_process_windows Windows process utilities
!! Windows implementation of process inspection for FPX.
!!
!! Uses ToolHelp32 API to inspect running processes without external tools.
!! No C wrappers, no shell calls.
!!
!! @ingroup group_process
!! @cond
#ifdef _WIN32
module fpx_process_windows
    use iso_c_binding
    use iso_fortran_env

    implicit none; private

    public :: get_process_name_windows, &
            get_parent_pid_windows

    integer(c_int32_t), parameter :: TH32CS_SNAPPROCESS = int(z'00000002', c_int32_t)
    integer(c_intptr_t), parameter :: INVALID_HANDLE = -1_c_intptr_t
    integer, parameter :: MAX_PATH = 260

    type, bind(C) :: PROCESSENTRY32W
        integer(c_int32_t)  :: dwSize
        integer(c_int32_t)  :: cntUsage
        integer(c_int32_t)  :: th32ProcessID
        integer(c_intptr_t) :: th32DefaultHeapID
        integer(c_int32_t)  :: th32ModuleID
        integer(c_int32_t)  :: cntThreads
        integer(c_int32_t)  :: th32ParentProcessID
        integer(c_int32_t)  :: pcPriClassBase
        integer(c_int32_t)  :: dwFlags
        integer(c_int16_t)  :: szExeFile(MAX_PATH)
    end type

    interface
        function GetCurrentProcessId() bind(C, name='GetCurrentProcessId')
            import; implicit none
            integer(c_int32_t) :: GetCurrentProcessId
        end function

        function CreateToolhelp32Snapshot(flags, pid) bind(C, name='CreateToolhelp32Snapshot')
            import; implicit none
            integer(c_int32_t), value :: flags
            integer(c_int32_t), value :: pid
            integer(c_intptr_t) :: CreateToolhelp32Snapshot
        end function

        function Process32FirstW(hSnap, pe) bind(C, name='Process32FirstW')
            import; implicit none
            integer(c_intptr_t), value :: hSnap
            type(PROCESSENTRY32W) :: pe
            integer(c_int) :: Process32FirstW
        end function

        function Process32NextW(hSnap, pe) bind(C, name='Process32NextW')
            import; implicit none
            integer(c_intptr_t), value :: hSnap
            type(PROCESSENTRY32W) :: pe
            integer(c_int32_t) :: Process32NextW
        end function

        function CloseHandle(h) bind(C, name='CloseHandle')
            import; implicit none
            integer(c_intptr_t), value :: h
            integer(c_int32_t) :: CloseHandle
        end function
    end interface

contains

    function utf16_to_string(w) result(str)
        integer(c_int16_t), intent(in) :: w(:)
        character(:), allocatable :: str
        !private
        integer :: i, n

        n = 0
        do i = 1, size(w)
            if (w(i) == 0) exit
            n = n + 1
        end do

        allocate(character(n) :: str)

        do i = 1, n
            str(i:i) = achar(int(w(i)))
        end do
    end function

    pure function strip_exe(name) result(out)
        character(*), intent(in) :: name
        character(:), allocatable :: out

        if (len(name) > 4) then
            if (name(len(name)-3:) == '.exe') then
                out = name(:len(name)-4)
                return
            end if
        end if

        out = name
    end function

    integer(c_int32_t) function current_pid() result(pid)
        pid = GetCurrentProcessId()
    end function

    integer(c_int32_t) function get_parent_pid_windows() result(ppid)
        !private
        type(PROCESSENTRY32W), target :: pe
        integer(c_intptr_t) :: snap
        integer(c_int32_t) :: ios, pid

        ppid = 0
        pid = current_pid()

        snap = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0)

        if (snap == INVALID_HANDLE) return

        pe%dwSize = c_sizeof(pe)

        if (Process32FirstW(snap, pe) /= 0) then

            do
                if (pe%th32ProcessID == pid) then
                    ppid = pe%th32ParentProcessID
                    exit
                end if

                if (Process32NextW(snap, pe) == 0) exit
            end do

        end if

        ios = CloseHandle(snap)
    end function

    function get_process_name_windows(pid) result(name)
        integer(c_int32_t), intent(in) :: pid
        character(:), allocatable :: name
        !private
        type(PROCESSENTRY32W), target :: pe
        integer(c_intptr_t) :: snap
        integer(c_int32_t) :: ios

        name = ''

        snap = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0_c_int32_t)
        if (snap == INVALID_HANDLE) return

        pe%dwSize = c_sizeof(pe)

        if (Process32FirstW(snap, pe) /= 0) then

            do
                if (pe%th32ProcessID == pid) then
                    name = utf16_to_string(pe%szExeFile)
                    exit
                end if

                if (Process32NextW(snap, pe) == 0) exit
            end do

        end if

        ios = CloseHandle(snap)

        name = strip_exe(name)
    end function
end module
#endif
!! @endcond