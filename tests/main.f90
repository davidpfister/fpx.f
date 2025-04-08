program main
    use fpx_parser
        use, intrinsic :: iso_fortran_env, only: stdout => output_unit, &
                                                 stderr => error_unit, &
                                                 stdin => input_unit

        call preprocess_file('tests/input.test', 'output.c')
end program