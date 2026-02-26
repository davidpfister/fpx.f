!> @file
!! @defgroup group_parser Parser
!! Fortran Preprocessor (fpx) – core parsing and preprocessing module
!!
!! This module implements a full-featured, modern Fortran preprocessor supporting:
!! - C-style line continuations with `\` and `\\`
!! - Fortran-style `&` continuations
!! - `#define`, `#undef`, object-like and function-like macros with variadic support
!! - `#include` with proper path resolution and recursion guard
!! - Conditional compilation: `#if`, `#ifdef`, `#ifndef`, `#elif`, `#else`, `#endif`
!! - C-style `/* ... */` comments (nestable aware)
!! - Macro expansion with argument substitution and stringification (`#`) / token-pasting (`##`)
!! - Interactive REPL mode when reading from stdin
!! - Multiple entry points for file-to-file, unit-to-unit, etc.
!!
!! The preprocessor is designed to be standards-conforming where possible while adding
!! useful extensions (variadic macros, better diagnostics, include path handling).
!!
!! <h2 class="groupheader">Examples</h2>
!!
!! 1. Preprocess a file to stdout:
!! @code{.f90}
!!    call preprocess('input.F90')
!! @endcode
!!
!! 2. Preprocess a file and write to another file:
!! @code{.f90}
!!    call preprocess('src/main.F90', 'preprocessed/main.F90')
!! @endcode
!!
!! 3. Use in a build system with unit numbers:
!! @code{.f90}
!!    integer :: iu, ou
!!    open(newunit=iu, file='input.F90')
!!    open(newunit=ou, file='output.F90')
!!    call preprocess(iu, ou)
!!    close(iu); close(ou)
!!    ...
!! @endcode
!!
!! 4. Interactive mode (stdin to stdout):
!! @code
!!    $ ./fpx
!!     [in]  #define PI 3.1415926535
!!     [out]
!!     [in]  real :: x = PI*2
!!     [out] real :: x = 3.1415926535*2
!!     [in]  (empty line or 'quit' to exit)
!! @endcode
module fpx_parser
    use, intrinsic :: iso_fortran_env, only: stdout => output_unit, iostat_end, stdin => input_unit
    use, intrinsic :: iso_c_binding, only: c_char, c_size_t, c_ptr, c_null_ptr, c_associated
    use fpx_constants
    use fpx_string
    use fpx_logging
    use fpx_macro
    use fpx_conditional
    use fpx_define
    use fpx_diagnostics
    use fpx_include
    use fpx_path
    use fpx_global
    use fpx_os

    implicit none; private

    public :: preprocess,  &
            global

    !> Generic interface to start preprocessing from various sources/sinks
    !!
    !! Allows preprocessing:
    !! - file to stdout
    !! - file to file
    !! - unit to file
    !! - unit to unit (most flexible, used internally for #include)
    !!
    !! @b Remarks
    !! @ingroup group_parser
    interface preprocess
        module procedure :: preprocess_file
        module procedure :: preprocess_file_to_unit
        module procedure :: preprocess_unit_to_file
        module procedure :: preprocess_unit_to_unit
    end interface

    character(256) :: name                              !< Current source file name (without path)
    logical        :: c_continue, f_continue            !< Flags for C-style and Fortran-style continuation
    logical        :: in_comment, reprocess, stitch     !< Internal state flags
    character(:), allocatable :: res, tmp               !< Accumulated result and temporary line buffers
    character(MAX_LINE_LEN)   :: line, continued_line   !< Raw and continued input line
    integer :: iline, icontinuation                     !< Current line number and continuation position

contains

    !> Preprocess a file and write result to an optional output file (default: stdout)
    !! Opens the input file, determines the base filename for error messages,
    !! opens the output file if requested, and delegates to the unit-to-unit routine.
    !! @param[in] filepath   Path to the input source file
    !! @param[in] outputfile Optional path to the output file; if absent output goes to stdout
    !!
    !! @b Remarks
    !! @ingroup group_parser
    subroutine preprocess_file(filepath, outputfile)
        character(*), intent(in)            :: filepath
        character(*), intent(in), optional  :: outputfile
        !private
        integer :: iunit, ierr, n, ounit
        character(len=1, kind=c_char) :: buf(256)

        open(newunit=iunit, file=filepath, status='old', action='read', iostat=ierr)
        if (ierr /= 0) then
            if (verbose) print *, "Error opening input file: ", trim(filepath)
            return
        else
            if (c_associated(getcwd_c(buf, size(buf, kind=c_size_t)))) then
                n = findloc(buf, achar(0), 1)
                name = filepath(n + 1:)
            end if
        end if

        if (present(outputfile)) then
            open(newunit=ounit, file=outputfile, status='replace', action='write', iostat=ierr)
            if (ierr /= 0) then
                if (verbose) print *, "Error opening output file: ", trim(outputfile)
                close(iunit)
                return
            end if
        else
            ounit = stdout
        end if

        call preprocess(iunit, ounit)
        if (iunit /= stdin) close(iunit)
        if (ounit /= stdout) close(ounit)
    end subroutine

    !> Preprocess from an already-open input unit and write to a file
    !! @param[in] iunit Input unit (must already be open for reading)
    !! @param[in] ofile Output filename
    !!
    !! @b Remarks
    !! @ingroup group_parser
    subroutine preprocess_unit_to_file(iunit, ofile)
        integer, intent(in)         :: iunit
        character(*), intent(in)    :: ofile
        !private
        integer :: ierr, ounit

        if (iunit /= stdin) then
            inquire(unit = iunit, name=name)
        end if

        open(newunit=ounit, file=ofile, status='replace', action='write', iostat=ierr)
        if (ierr /= 0) then
            if (verbose) print *, "Error opening output file: ", trim(ofile)
            close(iunit)
            return
        end if

        call preprocess(iunit, ounit)
        if (iunit /= stdin) close(iunit)
        if (ounit /= stdout) close(ounit)
    end subroutine

    !> Preprocess a file and write to an already-open output unit
    !! @param[in] ifile Input filename
    !! @param[in] ounit Output unit (already open for writing)
    !!
    !! @b Remarks
    !! @ingroup group_parser
    subroutine preprocess_file_to_unit(ifile, ounit)
        character(*), intent(in)    :: ifile
        integer, intent(in)         :: ounit
        !private
        integer :: iunit, ierr, n
        character(len=1, kind=c_char) :: buf(256)

        open(newunit=iunit, file=ifile, status='old', action='read', iostat=ierr)
        if (ierr /= 0) then
            if (verbose) print *, "Error opening input file: ", trim(ifile)
            return
        else
            if (c_associated(getcwd_c(buf, size(buf, kind=c_size_t)))) then
                n = findloc(buf, achar(0), 1)
                name = ifile(n + 1:)
            end if
        end if

        call preprocess(iunit, ounit)
        if (iunit /= stdin) close(iunit)
        if (ounit /= stdout) close(ounit)
    end subroutine

    !> Core preprocessing routine: read from iunit, write to ounit
    !! Sets up a clean macro environment for the top-level file,
    !! resets conditional compilation state, and calls the worker routine.
    !! @param[in] iunit Input unit
    !! @param[in] ounit Output unit
    !!
    !! @b Remarks
    !! @ingroup group_parser
    subroutine preprocess_unit_to_unit(iunit, ounit)
        integer, intent(in) :: iunit
        integer, intent(in) :: ounit
        !private
        type(macro), allocatable :: macros(:)

        if (.not. allocated(global%macros)) allocate(global%macros(0))
        allocate(macros(sizeof(global%macros)), source=global%macros)
        if (.not. allocated(global%undef)) allocate(global%undef(0))
        if (.not. allocated(global%includedir)) allocate(global%includedir(0))
        
        call add(global%macros, macro('__STDF__','1'))
        call add(global%macros, macro('__FPX__','1'))
        associate(os => get_os_type())
            if (os == OS_WINDOWS .or. os == OS_WINDOWSx86) then
                call add(global%macros, macro('_WIN32'))
                if (os /= OS_WINDOWSx86) call add(global%macros, macro('_WIN64'))
            end if
        end associate

        cond_depth = 0
        cond_stack(1)%active = .true.
        cond_stack(1)%has_met = .false.

        reprocess = .false.;  c_continue = .false.; f_continue = .false.
        icontinuation = 1; iline = 0
        continued_line = ''; res = ''

        call preprocess_unit(iunit, ounit, macros, .false.)
        deallocate(macros)
    end subroutine

    !> Worker routine that reads lines, handles continuations, comments and directives
    !! This is the main loop that:
    !! - reads lines with interactive prompt when iunit==stdin
    !! - handles both `\` and `&` continuations
    !! - strips or preserves comments appropriately
    !! - calls process_line() for directive processing and macro expansion
    !! - stitches lines when Fortran continuation (`&`) is active
    !! @param[in]    iunit       Input unit
    !! @param[in]    ounit       Output unit
    !! @param[inout] macros(:)   Current macro table (passed by value between include levels)
    !! @param[in]    from_include True if called recursively from #include
    !!
    !! @b Remarks
    !! @ingroup group_parser
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
            write(*, *)
            write(*, *) '   Welcome to fpx, the extended Fortran preprocessor. '
            write(*, *) '   The program can be exited at any time by hitting'
            write(*, *) "   'Enter' at the prompt without entering any data, "
            write(*, *) "   or with the 'quit' command."
        end if
        do
            if (interactive) write(*, '(/a)', advance='no') ' [in]  '  ! Command line prompt
            read(iunit, '(A)', iostat=ierr) line

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
                continued_line = continued_line(:icontinuation) // trim(adjustl(line))
            else
                continued_line = trim(adjustl(line))
            end if
            n = len_trim(continued_line); if (n == 0) cycle

            ! Check for line continuation with '\'
            if (verify(continued_line(n:n), '\') == 0) then
                ! Check for line break with '\\'
                if (continued_line(len_trim(continued_line) - 1:len_trim(continued_line)) == '\\' .and. global%line_break) then
                    c_continue = .true.
                    continued_line = continued_line(:len_trim(continued_line) - 2) // new_line('A')  ! Strip '\\'
                    icontinuation = len_trim(continued_line)
                else
                    c_continue = .true.
                    icontinuation = len_trim(continued_line) - 1
                    continued_line = continued_line(:icontinuation)
                end if
                cycle
            else
                c_continue = .false.

                tmp = process_line(continued_line, ounit, name, iline, macros, stitch)
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
                            write(ounit, '(A)') res
                            res = process_line(tmp, ounit, name, iline, macros, stitch)
                        else
                            res = process_line(concat(res, tmp), ounit, name, iline, macros, stitch)
                        end if
                        reprocess = .false.
                    else
                        res = trim(tmp)
                    end if
                    if (interactive) write(*, '(/a)', advance='no') ' [out] '  ! Command line prompt
                    write(ounit, '(A)') res
                    res = ''
                end if
            end if
        end do

        if (cond_depth > 0) then
            if (verbose) print *, "Error: Unclosed conditional block at end of file ", trim(name)
        end if
    end subroutine

    !> Process a single (possibly continued) line – handles directives and macro expansion
    !! Responsibilities:
    !! - Strip or terminate C-style block comments (`/* ... */`)
    !! - Detect and delegate preprocessor directives (`#define`, `#include`, conditionals, etc.)
    !! - Perform macro expansion when the line is in an active conditional block
    !! - Return whether the next line should be stitched (for Fortran `&` continuation inside macros)
    !! @param[in]    current_line Input line (already continued and trimmed)
    !! @param[in]    ounit        Output unit (used only for diagnostics inside called routines)
    !! @param[in]    filepath     Current file name (for error messages)
    !! @param[in]    linenum      Current line number (for error messages)
    !! @param[inout] macros(:)    Macro table
    !! @param[out]   stch         Set to .true. if the expanded line ends with `&` (stitch next line)
    !! @return                    Processed line (directives removed, macros expanded)
    !!
    !! @b Remarks
    !! @ingroup group_parser
    recursive function process_line(current_line, ounit, filepath, linenum, macros, stch) result(rst)
        character(*), intent(in)                :: current_line
        integer, intent(in)                     :: ounit
        character(*), intent(in)                :: filepath
        integer, intent(in)                     :: linenum
        type(macro), allocatable, intent(inout) :: macros(:)
        logical, intent(out)                    :: stch
        character(:), allocatable               :: rst
        !private
        character(:), allocatable :: trimmed_line
        logical :: active
        logical, save :: l_in_comment = .false.
        integer :: idx, comment_start, comment_end, n

        trimmed_line = trim(adjustl(current_line))
        rst = ''
        comment_end = index(trimmed_line, '*/')
        if (l_in_comment .and. comment_end > 0) then
            trimmed_line = trimmed_line(comment_end + 2:)
            l_in_comment = .false.
        end if

        if (l_in_comment) return
        comment_start = index(trimmed_line, '/*')
        if (comment_start > 0) then
            trimmed_line = trimmed_line(:comment_start - 1)
            l_in_comment = comment_end == 0
        end if
        n = len(trimmed_line); if (n == 0) return

        active = is_active()
        if (verbose) print *, "Processing line ", linenum, ": '", trim(trimmed_line), "'"
        if (verbose) print *, "is_active() = ", active, ", cond_depth = ", cond_depth
        if (head(trimmed_line) == '#') then
            if (len(trimmed_line) == 1) then
                return !null directive
            else if (starts_with(uppercase(adjustl(trimmed_line(2:))), 'DEFINE') .and. active) then
                call handle_define(trimmed_line, macros, 'DEFINE')
            else if (starts_with(uppercase(adjustl(trimmed_line(2:))), 'UNDEF') .and. active) then
                call handle_undef(trimmed_line, macros, 'UNDEF')
            else if (starts_with(uppercase(adjustl(trimmed_line(2:))), 'WARNING') .and. active) then
                call handle_warning(trimmed_line, macros, 'WARNING')
            else if (starts_with(uppercase(adjustl(trimmed_line(2:))), 'ERROR') .and. active) then
                call handle_error(trimmed_line, macros, 'ERROR')
            else if (starts_with(uppercase(adjustl(trimmed_line(2:))), 'INCLUDE') .and. active) then
                call handle_include(trimmed_line, ounit, filepath, linenum, preprocess_unit, macros, 'INCLUDE')
            else if (starts_with(uppercase(adjustl(trimmed_line(2:))), 'IFDEF')) then
                call handle_ifdef(trimmed_line, filepath, linenum, macros, 'IFDEF')
            else if (starts_with(uppercase(adjustl(trimmed_line(2:))), 'IFNDEF')) then
                call handle_ifndef(trimmed_line, filepath, linenum, macros, 'IFNDEF')
            else if (starts_with(uppercase(adjustl(trimmed_line(2:))), 'ELIFDEF')) then
                call handle_elifdef(trimmed_line, filepath, linenum, macros, 'ELIFDEF')
            else if (starts_with(uppercase(adjustl(trimmed_line(2:))), 'ELIFNDEF')) then
                call handle_elifndef(trimmed_line, filepath, linenum, macros, 'ELIFNDEF')
            else if (starts_with(uppercase(adjustl(trimmed_line(2:))), 'IF')) then
                call handle_if(trimmed_line, filepath, linenum, macros, 'IF')
            else if (starts_with(uppercase(adjustl(trimmed_line(2:))), 'ELIF')) then
                call handle_elif(trimmed_line, filepath, linenum, macros, 'ELIF')
            else if (starts_with(uppercase(adjustl(trimmed_line(2:))), 'ELSE')) then
                call handle_else(filepath, linenum)
            else if (starts_with(uppercase(adjustl(trimmed_line(2:))), 'ENDIF')) then
                call handle_endif(filepath, linenum)
            else if (starts_with(uppercase(adjustl(trimmed_line(2:))), 'PRAGMA') .and. active) then
                rst = trimmed_line
            end if
        else if (active) then
            if (.not. global%expand_macros) then
                rst = trimmed_line
            else
                rst = adjustl(expand_all(trimmed_line, macros, filepath, linenum, stch, global%extra_macros))
                if (verbose) print *, "Writing to output: '", trim(rst), "'"
            end if
        end if
    end function
end module
