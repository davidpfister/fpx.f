module fpx_parser
    use, intrinsic :: iso_fortran_env, only: stdout => output_unit, iostat_end
    use, intrinsic :: iso_c_binding, only: c_char, c_size_t,c_ptr, c_null_ptr, c_associated
    use fpx_constants
    use fpx_string
    use fpx_logging
    use fpx_macro
    use fpx_conditional
    use fpx_define
    use fpx_include
    use fpx_path
    use fpx_global

    implicit none; private

    public :: preprocess,  & 
              global

    interface preprocess
        module procedure :: preprocess_file
        module procedure :: preprocess_unit
    end interface
    
    character(256) :: filename
    logical :: c_continue, f_continue
    logical :: in_comment, reprocess, stitch
    character(:), allocatable :: res, tmp
    character(MAX_LINE_LEN) :: line, continued_line
    integer :: iline, icontinuation
    
contains

    subroutine preprocess_file(filepath, outputfile)
        character(*), intent(in)            :: filepath
        character(*), intent(in), optional  :: outputfile
        !private
        integer :: iunit, ierr, n, ounit
        type(macro), allocatable :: macros(:)
        character(len=1, kind=c_char) :: buf(256)

        open (newunit=iunit, file=filepath, status='old', action='read', iostat=ierr)
        if (ierr /= 0) then
            if (verbose) print *, "Error opening input file: ", trim(filepath)
            return
        else
            if (c_associated(getcwd_c(buf, size(buf, kind=c_size_t)))) then
               n = findloc(buf,achar(0),1)
               filename = filepath(n+1:)
            end if
        end if

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
        
        if (.not. allocated(global%macros)) allocate(global%macros(0))
        allocate (macros(sizeof(global%macros)), source = global%macros)
        cond_depth = 0
        cond_stack(1)%active = .true.
        cond_stack(1)%has_met = .false.
        
        reprocess = .false.;  c_continue = .false.; f_continue = .false.
        icontinuation = 1; iline = 0
        continued_line = ''; res = ''
        
        call preprocess_unit(iunit, ounit, macros, .false.)
        close (iunit)
        if (ounit /= stdout) close (ounit)
        deallocate (macros)
    end subroutine

    subroutine preprocess_unit(iunit, ounit, macros, from_include)
        integer, intent(in)                     :: iunit
        integer, intent(in)                     :: ounit
        type(macro), allocatable, intent(inout) :: macros(:)
        logical, intent(in)                     :: from_include
        !private
        integer :: ierr, n
        
        do
            read (iunit, '(A)', iostat=ierr) line
            if (ierr /= 0) then
                if (ierr == iostat_end .and. from_include) f_continue = tail(tmp) == '&'
                exit
            end if
            if (.not. from_include) iline = iline + 1

            if (c_continue) then
                continued_line = continued_line(:icontinuation)//trim(adjustl(line))
            else
                continued_line = trim(adjustl(line))
            end if
            n = len_trim(continued_line); if (n == 0) cycle

            ! Check for line continuation with '\'
            if (verify(continued_line(n:n), '\') == 0 ) then
                ! Check for line break with '\\'
                if (continued_line(len_trim(continued_line) - 1:len_trim(continued_line)) == '\\') then
                    c_continue = .true.
                    continued_line = continued_line(:len_trim(continued_line) - 2)//new_line('A') ! Strip '\\'
                    icontinuation = len_trim(continued_line)
                else
                    c_continue = .true.
                    icontinuation = len_trim(continued_line) - 1
                    continued_line = continued_line(:icontinuation)
                end if
                cycle
            else
                c_continue = .false.

                tmp = process_line(continued_line, ounit, filename, iline, macros, stitch)
                if (len_trim(tmp) == 0) cycle
                    
                in_comment = head(tmp) == '!'
                           
                if (merge(head(res) == '!', in_comment, len_trim(res) > 0)) then
                    f_continue = tail(tmp) == '&'
                else
                    if (in_comment .and. f_continue) cycle
                    f_continue = .not. in_comment .and. tail(tmp) == '&'
                end if
                
                if (f_continue .or. stitch) then
                    reprocess = .true.
                    res = concat(res, tmp)
                else
                    if (reprocess) then
                        if (.not. in_comment .and. head(res) == '!') then
                            write (ounit, '(A)') res
                            res = process_line(tmp, ounit, filename, iline, macros, stitch)
                        else
                            res = process_line(concat(res, tmp), ounit, filename, iline, macros, stitch)
                        end if
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
            if (verbose) print *, "Error: Unclosed conditional block at end of file ", trim(filename)
        end if
    end subroutine

    recursive function process_line(line, ounit, filename, iline, macros, stitch) result(res)
        character(*), intent(in)                :: line
        integer, intent(in)                     :: ounit
        character(*), intent(in)                :: filename
        integer, intent(in)                     :: iline
        character(:), allocatable               :: res
        type(macro), allocatable, intent(inout) :: macros(:)
        logical, intent(out)                    :: stitch
        !private
        character(:), allocatable :: trimmed_line
        logical :: active
        logical, save :: in_comment = .false.
        logical, save :: f_continue = .false.
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
                call handle_define(trimmed_line, macros)
            else if (starts_with(adjustl(trimmed_line(2:)), 'undef') .and. active) then
                call handle_undef(trimmed_line, macros)
            else if (starts_with(adjustl(trimmed_line(2:)), 'include') .and. active) then
                call handle_include(trimmed_line, ounit, filename, iline, preprocess_unit, macros)
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
            res = adjustl(expand_all(trimmed_line, macros, filename, iline, stitch))
            if (verbose) print *, "Writing to output: '", trim(res), "'"
        end if
    end function

end module
