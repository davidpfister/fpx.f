!> @brief Main entry point and command-line driver for the fpx Fortran preprocessor
!! This is the standalone executable program that parses command-line arguments
!! and launches the fpx preprocessing engine. It supports a rich set of options
!! compatible with traditional C/Fortran preprocessors while adding modern features.
!!
!! Supported command-line options:
!! - `-Dname` or `-Dname=value` → define macro (object-like)
!! - `-Uname`                   → undefine macro (add to global%undef)
!! - `-Ipath`                   → add directory to include search path
!! - `-o outfile`               → specify output file
!! - `-v`                       → print version and exit
!! - `-h`, `-?`                 → display help message and exit
!!
!! Input can be a file or stdin; output can be a file or stdout.
!! The program integrates fully with the fpx library: all global settings
!! (`global%macros`, `global%includedir`, etc.) are populated here.
!!
!! @par Examples
!!
!! 1. Basic preprocessing:
!!    $ fpx input.F90 -o output.f90
!!
!! 2. Define macros and add include path:
!!    $ fpx -DDEBUG=1 -DMPI -I./include src/main.F90
!!
!! 3. Predefine version and process from stdin:
!!    $ cat source.F90 | fpx -D_VERSION='"1.5.0"' -o preprocessed.f90
!!
!! 4. Interactive mode (stdin → stdout):
!!    $ fpx
!!    [in]  #define PI 3.14
!!    [out] 
!!    [in]  real :: r = PI
!!    [out] real :: r = 3.14
!!
!! 5. Show version or help:
!!    $ fpx -v
!!    $ fpx -h
#include <app.inc>
console(main)
    main(args)
        use, intrinsic :: iso_fortran_env, only: stdout => output_unit, &
                                                 stderr => error_unit, &
                                                 stdin => input_unit
        use fpx_macro
        use fpx_parser
        use fpx_string
        
        integer :: i, j, nargs
        character(:), allocatable :: infile, outfile
#if defined(_VERSION)
       character(*), parameter :: version = _VERSION
#else
       character(*), parameter :: version = '0.0.0'
#endif

        nargs = size(args)
        i = 1

        do while (i <= nargs)
            if (args(i)%chars(1:1) == '-') then
                select case(args(i)%chars(2:2))
                case ('D')
                    if (len(args(i)%chars) > 2) then
                        j = index(args(i)%chars(2:), '=')
                        if (j > 0) then
                            call add(global%macros, macro(args(i)%chars(3:j-1), args(i)%chars(j+1:)))
                        else
                            call add(global%macros, macro(args(i)%chars(3:)))
                        end if
                    end if
                case ('U')
                    if (.not. allocated(global%undef)) allocate(global%undef(0))
                    if (len(args(i)%chars) > 2) then
                        global%undef = [global%undef, string(args(i)%chars(3:))]
                    end if
                case ('I')
                    if (.not. allocated(global%includedir)) allocate(global%includedir(0))
                    if (len(args(i)%chars) > 2) then
                        global%includedir = [global%includedir, string(args(i)%chars(3:))]
                    end if
                case ('v')
                    write(*, '(*(A,/))') 'fpx version '//version,     &
                                         'Copyright (C) 2025 davidpfister', &
                                         'This is free software; see the source for copying conditions.  There is NO', &
                                         'warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.'
                    stop 0, quiet = .true.
                case ('h', '?')
                    write(*, '(*(A,/))') '                              fpx preprocessor help', &
                                         '                             =======================', &
                                         'fpx is a extended preprocessor for modern Fortran in Fortran.', &
                                         '', &
                                         '                             Preprocessor Option List', &
                                         '                             -----------------------', &
                                         '-D<macro>         Define a <macro> with no value.', &
                                         '-D<macro>=<val>   Define a <macro> with <val> as its value.', & 
                                         '-U<macro>         Undefine <macro>', &
                                         '-I<dir>           Add <dir> to the end of the global include paths.', & 
                                         '-h, -?            Display this help', &
                                         '-o                Output file path with name and extension.', &
                                         '-v                Display the version of the program.'
                    stop 0, quiet = .true.
                case ('o')
                    outfile = args(i)
                end select
            else
                if (allocated(infile)) then
                    outfile = args(i)
                else
                    infile = args(i)
                end if
            end if
            i = i + 1
        end do
    
        if (allocated(infile)) then
            if (allocated(outfile)) then
                call preprocess(trim(infile), trim(outfile))
            else
                call preprocess(trim(infile), stdout)
            end if
        else
            if (allocated(outfile)) then
                call preprocess(stdin, trim(outfile))
            else
                call preprocess(stdin, stdout)
            end if
        end if

    endmain
end

