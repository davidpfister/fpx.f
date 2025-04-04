#include <app.inc>
console(fpx)
    main(args)
        use fpx_parser
        use, intrinsic :: iso_fortran_env, only: stdout => output_unit, &
                                                 stderr => error_unit, &
                                                 stdin => input_unit

        call preprocess_file(args(1)%chars, 'output.c')

    endmain
end
