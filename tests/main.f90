program test
    use fpx_parser
        use, intrinsic :: iso_fortran_env, only: stdout => output_unit, &
                                                 stderr => error_unit, &
                                                 stdin => input_unit
#ifdef _FPM
        call preprocess_file('tests/input.in', 'tests/output.out')
#else
        call preprocess_file('input.in', 'output.out')
#endif
end program