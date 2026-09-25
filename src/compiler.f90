!> @file
!! @defgroup group_fpx_compiler Compiler
!! Retrieves compiler information at run time as the compiler's name
!! and version. It uses the intrinsic
!! functions `compiler_options` and `compiler_version` from the
!! module `iso_fortran_env`
module fpx_compiler
    use, intrinsic :: iso_fortran_env, only: compiler_version, compiler_options
    use fpx_os, only: get_os_type, OS_LINUX, OS_MACOS, OS_WINDOWS
    use fpx_string, only: lowercase
    
    implicit none; private
    
    public :: get_compiler

contains

    function get_compiler(is_standalone) result(res)
        logical, intent(in) :: is_standalone
        character(:), allocatable :: res
        !private
        integer :: ios, l
        
        if (is_standalone) then
            call get_environment_variable('FC', length=l, status = ios, trim_name = .true.)
            select case (ios)
            case(1)
                res = get_caller_name()
            case(2)
                res = ''
            case default
                allocate(character(len=max(l, 1)) :: res)
                call get_environment_variable('FC', res, status = ios, trim_name = .true.)
                if (ios /= 0) res = ''
            end select
        else
            res = get_compilerinfo()
        end if
    end function
    
    !> Detects current compiler and prints name, vendor, and version
    !! @ingroup group_steps_compiler
    !! @b Remarks
    function get_compilerinfo() result(res)
        character(:), allocatable :: res
        !private
        integer :: os
        character(:), allocatable :: version
    
        os = get_os_type()
        version = lowercase(compiler_version())
    
        if (index(version, 'gcc') > 0) then
            res = 'gfortran'
        else if (index(version, 'intel') > 0) then
            if (index(version, 'classic') > 0) then
                if (os == OS_LINUX) then
                    res = 'ifort'
                elseif (os == OS_WINDOWS) then
                    res = merge('ia32 ', 'ifort', index(version, 'IA-32') > 0)
                elseif (os == OS_MACOS) then
                    res = 'ifort'
                end if
            else
                if (os == OS_LINUX) then
                    res = 'ifx'
                elseif (os == OS_WINDOWS) then
                    res = 'ifx'
                else
                    res = 'ifx'
                end if
            end if
        else if (index(version, 'nvfortran') > 0) then
            res = 'nvhpc'
        else if (index(version, 'nag fortran') > 0) then
            res = 'nagfor'
        else if (index(version, 'flang') > 0) then
            res = 'flang'
        else if (index(version, 'ibm xl') > 0) then
            res = 'xlf90'
        else if (index(version, 'lahey/fujitsu') > 0) then
            res = 'lf90'
        else if (index(version, 'lfortran') > 0) then
            res = 'lfortran'
        end if
    end function
    
    function get_caller_name() result(res)
#ifdef _WIN32
        use fpx_process_windows, only: get_parent_pid_windows, get_process_name_windows
        character(:), allocatable :: res
        !private
        integer :: pid

        pid = 1
        
        do while (pid > 0)
            pid = get_parent_pid_windows()
            if (pid <= 1) then
                res = ''
                exit
            end if
            res = get_process_name_windows(pid)
            if (is_compiler(res)) exit
        end do
#else
        use fpx_process_posix, only: get_parent_pid_posix, get_process_name_posix
        !private
        character(:), allocatable :: res
        !private
        integer :: pid
        
        pid = 1
        
        do while (pid > 0)
            pid = get_parent_pid_posix()
            if (pid <= 1) then
                res = ''
                exit
            end if
            res = get_process_name_posix(pid)
            if (is_compiler(res)) exit
        end do
#endif
    end function

    !> Return a normalized executable name.
    !!
    !! The returned name is lowercase, platform-independent and stripped of
    !! common executable suffixes. Equivalent compiler executables are mapped
    !! to a canonical name.
    !!
    !! Examples:
    !! - `gfortran.exe` to `gfortran`
    !! - `ifx.exe` to `ifx`
    !! - `flang-new` to `flang`
    !! - `NVFORTRAN.EXE` to `nvfortran`
    !!
    !! @return Canonical executable name.
    !! @ingroup group_process
    function normalized_name(name) result(res)
        character(:), allocatable, intent(in)   :: name
        character(:), allocatable :: res
        !private 
        integer :: pos
        
        if (.not. allocated(name)) then
            res = ''
            return
        end if
        pos = index(name, '.')
        
        if (pos > 0) then
            res = name(:pos-1)
        else
            res = name
        end if
        res = lowercase(res)
    end function


    !> Determine whether the executable is a known Fortran compiler.
    !!
    !! The comparison is performed using the normalized executable name.
    !!
    !! @return `.true.` if the executable corresponds to a supported Fortran
    !! compiler.
    !! @ingroup group_process
    logical function is_compiler(name)
        character(:), allocatable, intent(in)   :: name
        !private
        character(:), allocatable       :: exe

        exe = normalized_name(name)

        select case (exe)
        case ('gfortran',      &
              'caf',           &
              'ifort',         &
              'ifx',           &
              'ia32',          &
              'pgfortran',     &   
              'nvfortran',     &
              'f95',           &
              'f18',           &
              'flang-new',     &
              'flang',         &
              'lfortran',      &
              'nagfor',        &
              'xlf90',         &
              'lf95',          &
              'crayftn')

            is_compiler = .true.
        case default
            is_compiler = .false.
        end select
    end function
end module
