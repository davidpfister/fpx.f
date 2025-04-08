program test
    use fpx_parser
        use, intrinsic :: iso_fortran_env, only: stdout => output_unit, &
                                                 stderr => error_unit, &
                                                 stdin => input_unit
#ifdef _WIN32
        call preprocess_file('input.in', 'output.c')
#else
        call preprocess_file('tests/input.in', 'output.c')
#endif
end program