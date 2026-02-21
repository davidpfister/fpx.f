#include <assertion.inc>
TESTPROGRAM(main)
    block
        use fpx_parser
        use test_utils
        use fpx_path
        use fpx_macro

        character(256), allocatable  :: files(:)
        character(:), allocatable :: ref
        integer :: i, j

        TEST_PRINT('lfortran_unit_test')

#ifdef _FPM
        call chdir(join('tests','lfortran'))
#endif
        call getfiles('', files)
        call add(global%macros, macro('__LFORTRAN__','1'))
        call add(global%macros, macro('__VERSION__'))
        call add(global%macros, macro('__LFORTRAN_MAJOR__'))
        call add(global%macros, macro('__LFORTRAN_MINOR__'))
        call add(global%macros, macro('__LFORTRAN_PATCHLEVEL__'))

        do i = 1, size(files)
            if (filename(files(i), .true.) == 'main.f90') &
                cycle
            if (index(files(i), '.out') > 0) then
                cycle
            else if (index(files(i), '.ref') > 0) then
                cycle
            else if (index(files(i), '.f90') > 0 .or. index(files(i), '.F90') > 0) then
                call preprocess(trim(files(i)), trim(files(i))//'.out')

                TEST(filename(files(i)))
                    logical :: exists
                    integer :: ierr, unit, cunit
                    character(256), allocatable :: actual(:), expected(:)
                    inquire(file=trim(files(i))//'.out', exist=exists)
#if defined (_DEBUG) || defined (DEBUG)
                    ASSERT_TRUE(exists)
#endif
                    ref = files(i); ref(index(ref, '.', back = .true.) + 1:) = 'ref'
                    ref = trim(ref)
                    call getlines(trim(files(i))//'.out', actual, .false.)
                    call getlines(ref, expected, .false.)

                    EXPECT_EQ(size(actual), size(expected))

                    do j = 1, min(size(actual), size(expected))
                        EXPECT_STREQ(trim(actual(j)), trim(expected(j)))
                    end do
                END_TEST
            end if
        end do
        call clear(global%macros)
#ifdef _FPM
        call chdir(join('..','..'))
#endif
    end block
END_TESTPROGRAM