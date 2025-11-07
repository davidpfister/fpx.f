module fpx_parser
    use, intrinsic :: iso_fortran_env, only: stdout => output_unit, iostat_end, stdin => input_unit
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
        module procedure :: preprocess_file_to_unit
        module procedure :: preprocess_unit_to_file
        module procedure :: preprocess_unit_to_unit
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
        
        call preprocess(iunit, ounit)
        if (iunit /= stdin) close (iunit)
        if (ounit /= stdout) close (ounit)
    end subroutine
    
    subroutine preprocess_unit_to_file(iunit, ofile)
        integer, intent(in)         :: iunit
        character(*), intent(in)    :: ofile
        !private
        integer :: ierr, ounit

        if (iunit /= stdin) then
            inquire(unit = iunit, name = filename)
        end if
 
        open (newunit=ounit, file=ofile, status='replace', action='write', iostat=ierr)
        if (ierr /= 0) then
            if (verbose) print *, "Error opening output file: ", trim(ofile)
            close (iunit)
            return
        end if
        
        call preprocess(iunit, ounit)
        if (iunit /= stdin) close (iunit)
        if (ounit /= stdout) close (ounit)
    end subroutine
    
    subroutine preprocess_file_to_unit(ifile, ounit)
        character(*), intent(in)    :: ifile
        integer, intent(in)         :: ounit
        !private
        integer :: iunit, ierr, n
        character(len=1, kind=c_char) :: buf(256)

        open (newunit=iunit, file=ifile, status='old', action='read', iostat=ierr)
        if (ierr /= 0) then
            if (verbose) print *, "Error opening input file: ", trim(ifile)
            return
        else
            if (c_associated(getcwd_c(buf, size(buf, kind=c_size_t)))) then
               n = findloc(buf,achar(0),1)
               filename = ifile(n+1:)
            end if
        end if
        
        call preprocess(iunit, ounit)
        if (iunit /= stdin) close (iunit)
        if (ounit /= stdout) close (ounit)
    end subroutine
    
    subroutine preprocess_unit_to_unit(iunit, ounit)
        integer, intent(in) :: iunit
        integer, intent(in) :: ounit
        !private
        type(macro), allocatable :: macros(:)
        
        if (.not. allocated(global%macros)) allocate(global%macros(0))
        allocate (macros(sizeof(global%macros)), source = global%macros)
        cond_depth = 0
        cond_stack(1)%active = .true.
        cond_stack(1)%has_met = .false.
        
        reprocess = .false.;  c_continue = .false.; f_continue = .false.
        icontinuation = 1; iline = 0
        continued_line = ''; res = ''
        
        call preprocess_unit(iunit, ounit, macros, .false.)
        deallocate (macros)
    end subroutine

    subroutine preprocess_unit(iunit, ounit, macros, from_include)
        integer, intent(in)                     :: iunit
        integer, intent(in)                     :: ounit
        type(macro), allocatable, intent(inout) :: macros(:)
        logical, intent(in)                     :: from_include
        !private
        integer :: ierr, n
        character(:), allocatable :: uline
        logical :: interactive
        
        interactive = iunit == stdin
        
        if (interactive) then
            write (*, *)
            write (*, *) '   Welcome to fpx, the extended Fortran preprocessor. '
            write (*, *) '   The program can be exited at any time by hitting'
            write (*, *) "   'Enter' at the prompt without entering any data, "
            write (*, *) "   or with the 'quit' command."
        end if
        do
            if (interactive) write (*, '(/a)', advance='no') ' [in]  ' ! Command line prompt 
            read (iunit, '(A)', iostat=ierr) line
            
            if (interactive) then
                if (line == '') exit
                uline = uppercase(trim(adjustl(line)))
                if (uline == 'QUIT') exit
            end if
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
                    icontinuation = len_trim(continued_line)-1
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
                    if (interactive) write (*, '(/a)', advance='no') ' [out] ' ! Command line prompt 
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
            if (starts_with(uppercase(adjustl(trimmed_line(2:))), 'DEFINE') .and. active) then
                call handle_define(trimmed_line, macros, 'DEFINE')
            else if (starts_with(uppercase(adjustl(trimmed_line(2:))), 'UNDEF') .and. active) then
                call handle_undef(trimmed_line, macros, 'UNDEF')
            else if (starts_with(uppercase(adjustl(trimmed_line(2:))), 'INCLUDE') .and. active) then
                call handle_include(trimmed_line, ounit, filename, iline, preprocess_unit, macros, 'INCLUDE')
            else if (starts_with(uppercase(adjustl(trimmed_line(2:))), 'IFDEF')) then
                call handle_ifdef(trimmed_line, filename, iline, macros, 'IFDEF')
            else if (starts_with(uppercase(adjustl(trimmed_line(2:))), 'IFNDEF')) then
                call handle_ifndef(trimmed_line, filename, iline, macros, 'IFNDEF')
            else if (starts_with(uppercase(adjustl(trimmed_line(2:))), 'IF')) then
                call handle_if(trimmed_line, filename, iline, macros, 'IF')
            else if (starts_with(uppercase(adjustl(trimmed_line(2:))), 'ELIF')) then
                call handle_elif(trimmed_line, filename, iline, macros, 'ELIF')
            else if (starts_with(uppercase(adjustl(trimmed_line(2:))), 'ELSE')) then
                call handle_else(filename, iline)
            else if (starts_with(uppercase(adjustl(trimmed_line(2:))), 'ENDIF')) then
                call handle_endif(filename, iline)
            end if
        else if (active) then
            if (.not. global%expand_macros) then 
                res = trimmed_line
            else
                res = adjustl(expand_all(trimmed_line, macros, filename, iline, stitch))
                if (verbose) print *, "Writing to output: '", trim(res), "'"
            end if
            
        end if
    end function

end module
