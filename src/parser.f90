module fpx_parser
    use, intrinsic :: iso_fortran_env, only: stdout => output_unit
    use, intrinsic :: iso_c_binding, only: c_char, c_size_t,c_ptr, c_null_ptr, c_associated
    use fpx_constants
    use fpx_logging
    use fpx_macro
    use fpx_conditional
    use fpx_define
    use fpx_include
    use fpx_path
    use fpx_global

    implicit none; private

    public :: preprocess,  & 
              global, global_t

    type(macro_t), allocatable :: macros(:)
    integer :: num_macros = 0

    interface preprocess
        module procedure :: preprocess_file
        module procedure :: preprocess_unit
    end interface
    
contains

    subroutine preprocess_file(filepath, outputfile)
        character(*), intent(in)            :: filepath
        character(*), intent(in), optional  :: outputfile
        !private
        integer :: iunit, ierr

        open (newunit=iunit, file=filepath, status='old', action='read', iostat=ierr)
        if (ierr /= 0) then
            if (verbose) print *, "Error opening input file: ", trim(filepath)
            return
        end if
        call preprocess_unit(iunit, outputfile)
    end subroutine

    subroutine preprocess_unit(iunit, outputfile)
        integer, intent(in)                 :: iunit
        character(*), intent(in), optional  :: outputfile
        !private
        character(MAX_LINE_LEN) :: line, continued_line
        character(256) :: filepath
        character(:), allocatable :: res, tmp
        character(len=1, kind=c_char) :: buf(256)
        integer :: ierr, iline, n, ounit, icontinuation
        logical :: in_continuation1, in_continuation2
        logical :: is_dir, in_comment, reprocess

        icontinuation = 1
        in_continuation2 = .false.
        reprocess = .false.
        is_dir = .false.
        if (present(outputfile)) then
            open (newunit=ounit, file=outputfile, status='replace', action='write', iostat=ierr)
            if (ierr /= 0) then
                if (verbose) print *, "Error opening output file: ", trim(outputfile)
                close (iunit)
                return
            end if
        else
            ounit = stdout
        end if

        inquire (unit=iunit, name=filepath)
        
        if (c_associated(getcwd_c(buf, size(buf, kind=c_size_t)))) then
           n = findloc(buf,achar(0),1)
           filepath = filepath(n+1:)
        end if

        if (.not. allocated(global)) global = global_t()
        if (allocated(macros)) deallocate (macros)
        num_macros = size(global%macros)
        allocate (macros(num_macros), source = global%macros)
        cond_depth = 0
        cond_stack(1)%active = .true.
        cond_stack(1)%has_met = .false.

        iline = 0
        in_continuation1 = .false.
        in_continuation2 = .false.
        is_dir = .false.
        continued_line = ''; res = ''
        
        do
            read (iunit, '(A)', iostat=ierr) line; if (ierr /= 0) exit
            iline = iline + 1

            if (in_continuation1) then
                continued_line = continued_line(:icontinuation)//trim(adjustl(line))
            else
                continued_line = trim(adjustl(line))
            end if
            n = len_trim(continued_line); if (n == 0) cycle

            ! Check for line continuation with '\'
            if (verify(continued_line(n:n), '\') == 0 ) then
                ! Check for line break with '\\'
                if (continued_line(len_trim(continued_line) - 1:len_trim(continued_line)) == '\\') then
                    in_continuation1 = .true.
                    continued_line = continued_line(:len_trim(continued_line) - 2)//new_line('A') ! Strip '\\'
                    icontinuation = len_trim(continued_line)
                else
                    in_continuation1 = .true.
                    icontinuation = len_trim(continued_line) - 1
                    continued_line = continued_line(:icontinuation)
                end if
                cycle
            else
                in_continuation1 = .false.; in_comment = .false.

                tmp = process_line(continued_line, ounit, filepath, iline)
                n = len_trim(tmp); if (n == 0) cycle
                    
                select case (head(tmp))
                case('&')
                    tmp = tmp(2:)
                    n = n - 1
                case('!')
                    in_comment = .true.
                end select
            
                if (merge(head(res) == '!', in_comment, len_trim(res) > 0)) then
                    in_continuation2 = merge(.true., .false., tmp(n:n) == '&')
                else
                    if (in_comment) then
                        if (in_continuation2) cycle
                    end if
                    in_continuation2 = merge(.true., .false., .not. in_comment .and. tmp(n:n) == '&')
                end if
                
                if (in_continuation2) then
                    reprocess = .true.
                    if (tail(tmp(:n-1)) == '(') then
                        res = res // trim(tmp(:n-1))
                    else
                        res = res // tmp(:n-1)
                    end if
                else
                    if (reprocess) then
                        res = process_line(res // trim(tmp), ounit, filepath, iline)
                        reprocess = .false.
                    else
                        res = trim(tmp)
                    end if
                    write (ounit, '(A)') res
                    res = ''
                end if
            end if
        end do

        if (cond_depth > 0) then
            if (verbose) print *, "Error: Unclosed conditional block at end of file ", trim(filepath)
        end if

        close (iunit)
        if (ounit /= stdout) close (ounit)
        deallocate (macros)
    end subroutine

    recursive function process_line(line, ounit, filename, iline) result(res)
        character(*), intent(in)    :: line
        integer, intent(in)         :: ounit
        character(*), intent(in)    :: filename
        integer, intent(in)         :: iline
        character(:), allocatable   :: res
        !private
        character(:), allocatable :: trimmed_line
        logical :: active
        logical, save :: in_comment = .false.
        logical, save :: in_continuation2 = .false.
        integer :: idx, comment_start, comment_end, n

        trimmed_line = trim(adjustl(line))
        res = ''
        comment_end = index(trimmed_line, '*/')
        if (in_comment .and. comment_end > 0) then
            trimmed_line = trimmed_line(comment_end + 2:)
            in_comment = .false.
        end if
        
        if (in_comment) return
        comment_start = index(trimmed_line, '/*')
        if (comment_start > 0) then
            trimmed_line = trimmed_line(:comment_start - 1)
            in_comment = comment_end == 0
        end if
        n = len(trimmed_line); if (n == 0) return

        active = is_active()
        if (verbose) print *, "Processing line ", iline, ": '", trim(trimmed_line), "'"
        if (verbose) print *, "is_active() = ", active, ", cond_depth = ", cond_depth
        if (head(trimmed_line) == '#') then
            if (starts_with(adjustl(trimmed_line(2:)), 'define') .and. active) then
                call handle_define(trimmed_line, num_macros, macros)
            else if (starts_with(adjustl(trimmed_line(2:)), 'undef') .and. active) then
                call handle_undef(trimmed_line, num_macros, macros)
            else if (starts_with(adjustl(trimmed_line(2:)), 'include') .and. active) then
                call handle_include(trimmed_line, ounit, filename, iline, process_line)
            else if (starts_with(adjustl(trimmed_line(2:)), 'ifdef')) then
                call handle_ifdef(trimmed_line, filename, iline, macros)
            else if (starts_with(adjustl(trimmed_line(2:)), 'ifndef')) then
                call handle_ifndef(trimmed_line, filename, iline, macros)
            else if (starts_with(adjustl(trimmed_line(2:)), 'if')) then
                call handle_if(trimmed_line, filename, iline, macros)
            else if (starts_with(adjustl(trimmed_line(2:)), 'elif')) then
                call handle_elif(trimmed_line, filename, iline, macros)
            else if (starts_with(adjustl(trimmed_line(2:)), 'else')) then
                call handle_else(filename, iline)
            else if (starts_with(adjustl(trimmed_line(2:)), 'endif')) then
                call handle_endif(filename, iline)
            end if
        else if (active) then           
            res = adjustl(expand_all(trimmed_line, macros, filename, iline))
            if (verbose) print *, "Writing to output: '", trim(res), "'"
        end if
    end function

end module
