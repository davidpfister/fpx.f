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
        character(len=1, kind=c_char) :: buf(256)
        integer :: ierr, iline, n, ounit, icontinuation
        logical :: in_continuation

        icontinuation = 1
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
        in_continuation = .false.
        continued_line = ''
        
        do
            read (iunit, '(A)', iostat=ierr) line
            if (ierr /= 0) exit
            iline = iline + 1

            if (in_continuation) then
                continued_line = continued_line(:icontinuation)//trim(adjustl(line))
            else
                continued_line = trim(adjustl(line))
            end if

            if (len_trim(continued_line) == 0) cycle

            ! Check for line continuation with '\'
            if (verify(continued_line(len_trim(continued_line):len_trim(continued_line)), '\') == 0) then
                ! Check for line break with '\\'
                if (continued_line(len_trim(continued_line) - 1:len_trim(continued_line)) == '\\') then
                    in_continuation = .true.
                    continued_line = continued_line(:len_trim(continued_line) - 2)//new_line('A') ! Strip '\\'
                    icontinuation = len_trim(continued_line)
                else
                    in_continuation = .true.
                    icontinuation = len_trim(continued_line) - 1
                    continued_line = continued_line(:icontinuation)
                end if
                cycle
            else
                in_continuation = .false.
                call process_line(continued_line, ounit, filepath, iline)
            end if
        end do

        if (cond_depth > 0) then
            if (verbose) print *, "Error: Unclosed conditional block at end of file ", trim(filepath)
        end if

        close (iunit)
        if (ounit /= stdout) close (ounit)
        deallocate (macros)
    end subroutine

    recursive subroutine process_line(line, ounit, filename, iline)
        character(*), intent(in)    :: line
        integer, intent(in)         :: ounit
        character(*), intent(in)    :: filename
        integer, intent(in)         :: iline
        !private
        character(:), allocatable :: trimmed_line, expanded_line
        logical :: active
        logical, save :: in_comment = .false.
        integer :: idx, comment_start, comment_end, n

        trimmed_line = trim(adjustl(line))

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
        if (trimmed_line(1:1) == '#') then
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
            if (len_trim(trimmed_line) == 0) return
            !if (trimmed_line(1:1) == '!') return
            expanded_line = expand_all(trimmed_line, macros, filename, iline)
            if (verbose) print *, "Writing to output: '", trim(expanded_line), "'"
            !if (len_trim(expanded_line) == 0) return
            !if (expanded_line(1:1) == '!') return
            !if (expanded_line(len_trim(expanded_line):len_trim(expanded_line)) == '&') then
            !    expanded_line(len_trim(expanded_line):len_trim(expanded_line)) = ' '
            !    if (starts_with(expanded_line, '&', idx)) expanded_line(idx:idx) = ' '
            !    write (ounit, '(A)', advance='no') trim(expanded_line)
            !else
            !    if (starts_with(expanded_line, '&', idx)) expanded_line(idx:idx) = ' '
                write (ounit, '(A)') trim(expanded_line)
            !end if
        end if
    end subroutine

end module
