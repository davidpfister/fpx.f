module  test_utils
    use fpx_path

    implicit none; private

    public :: getfiles,     &
            readruns,     &
            readline,     &
            getlines

contains

    subroutine getfiles(dirpath, res)
        character(*), intent(in)                    :: dirpath
        character(256), allocatable, intent(out)    :: res(:)
        !private
        integer :: ierr, unit
        character(:), allocatable :: line
        character(256) :: tmp
#ifdef _WIN32
        call execute_command_line('dir ' // dirpath // '  /b /s > filecontent.txt')
#else
        call execute_command_line('ls *.?* ' // dirpath // ' > filecontent.txt')
#endif
        allocate(res(0))

        open(newunit=unit, file='filecontent.txt', action='read')
        do while (.true.)
            call readline(unit, line, ierr)
            if (ierr /= 0) exit
            if (len_trim(line) == 0) cycle
            tmp = ' '; tmp = trim(line)
            res = [res, tmp]
        end do
        close(unit)
    end subroutine

    subroutine getlines(filepath, res, keepall)
        character(*), intent(in)                    :: filepath
        character(256), allocatable, intent(out)    :: res(:)
        logical, intent(in), optional                           :: keepall
        !private
        logical :: exists, keep
        integer :: idx, ierr, count, k, unit, last, pos
        character(:), allocatable :: line
        character(256) :: tmp
        character(1), parameter :: LF = char(10)            !< line feed.
        character(1), parameter :: CR = char(13)            !< carriage return.
        character(2), parameter :: CRLF = CR // LF            !< carriage return and line feed new line.

        if (present(keepall)) keep = keepall
        allocate(res(0))

        inquire(file=filepath, exist=exists)
        if (.not. exists) return

        open(newunit=unit, file=filepath, status='old', action='read', iostat=ierr)
        if (ierr /= 0) return
        count = 0

        do while (.true.)
            call readline(unit, line, ierr)
            if (ierr /= 0) exit

            if (.not. keep .and. len_trim(line) == 0) cycle
            tmp = '  '; tmp = trim(adjustl(line))
            if (.not. keep .and. (tmp(1:1) == '!' .or. tmp(1:1) == '#' .or. tmp(1:2) == '//')) cycle
            pos = index(line, CR); if (pos > 0) tmp(pos:pos) = ' '
            pos = index(line, LF); if (pos > 0) tmp(pos:pos) = ' '
            res = [res, tmp]
            count = count + 1
        end do
        close(unit)
    end subroutine

    subroutine readruns(filepath, res)
        character(*), intent(in)                    :: filepath
        character(:), allocatable, intent(out)    :: res
        !private
        logical :: exists
        integer :: idx, ierr, unit
        character(:), allocatable :: line

        allocate(character(0) :: res)

        inquire(file=filepath, exist=exists)
        if (.not. exists) return

        open(newunit=unit, file=filepath, status='old', action='read', iostat=ierr)
        if (ierr /= 0) return
        do while (.true.)
            call readline(unit, line, ierr)
            if (ierr /= 0) exit
            idx = index(line, 'RUN'); if (idx <= 0) cycle
            idx = index(line, ':')
            res = res // trim(adjustl(line(idx + 1:)))
        end do
        close(unit)

    end subroutine

    subroutine readline(iunit, line, ierr)
        integer, intent(in)                     :: iunit
        character(:), allocatable, intent(out)  :: line
        integer, intent(out)                    :: ierr
        !private
        character(1024) :: buffer
        integer :: last, isize

        line = ''; ierr = 0
        do
            read(unit=iunit, iostat=ierr, fmt='(A)', advance='no', size=isize) buffer
            if (isize > 0) line = line // buffer(:isize)
            if (is_iostat_eor(ierr)) then
                last = len(line)
                if (last /= 0) then
                    if (line(last:last) == '\') then
                        line = line(:last - 1); cycle
                    end if
                end if
                ierr = 0; exit
            elseif (ierr /= 0) then
                exit
            end if
        end do

        line = trim(line)
    end subroutine

end module
