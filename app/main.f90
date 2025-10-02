#include <app.inc>
console(main)
    main(args)
        use, intrinsic :: iso_fortran_env, only: stdout => output_unit, &
                            stderr => error_unit
        
        integer :: i, nargs
        character(:), allocatable :: option
#if defined(_VERSION)
       character(*), parameter :: version = _VERSION
#else
       character(*), parameter :: version = '0.0.0'
#endif

        nargs = size(args)
        i = 2

        do while (i <= nargs)
            select case(args(i)%chars(1:2))
            case ('-D')
                i = i + 1
                if (i <= nargs) option = args(i)
            case ('-U')
                i = i + 1
                if (i <= nargs) option = args(i)
            case ('-I')
                i = i + 1
                if (i <= nargs) option = args(i)
            case ('-Y')
                i = i + 1
                if (i <= nargs) option = args(i)
            case ('-v')
                i = i + 1
                write(*, '(*(A,/))') 'fpx version '//version,     &
                                     'Copyright (C) 2025 davidpfister', &
                                     'This is free software; see the source for copying conditions.  There is NO', &
                                     'warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.'
                stop
            case ('-h', '-?')
                i = i + 1
                write(*, '(*(A,/))') '                              fpx preprocessor help', &
                                     '                             =======================', &
                                     'fpx is a extended preprocessor for modern Fortran in Fortran.', &
                                     '', &
                                     '                             Preprocessor Option List', &
                                     '                             -----------------------', &
                                     '-D                ', & 
                                     '-U                ', &
                                     '-I                ', & 
                                     '-Y                ', &
                                     '-h, -?            ', &
                                     '-o                Output file path with name and extension.', &
                                     '-v                Display the version of the program.'
                stop 0, quiet = .true.
            end select
            i = i + 1
        end do

    endmain
end

