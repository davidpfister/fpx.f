#include <assertion.inc>
TESTPROGRAM(main)

    TEST('test_expression')
        use fpx_token
        use fpx_macro

        type(macro) :: macros(0)
        integer :: res

        EXPECT_TRUE(evaluate_expression('3-2', macros, res))
        EXPECT_EQ(res, 3-2)

        EXPECT_TRUE(evaluate_expression('3-1', macros, res))
        EXPECT_EQ(res, 3-1)

        EXPECT_TRUE(evaluate_expression('(6*5-5)/5', macros, res))
        EXPECT_EQ(res, (6*5-5)/5)

        EXPECT_TRUE(evaluate_expression('(6*5-6)/5', macros, res))
        EXPECT_EQ(res, (6*5-6)/5)

        EXPECT_TRUE(evaluate_expression('(5+6*5)/5', macros, res))
        EXPECT_EQ(res, (5+6*5)/5)

        EXPECT_TRUE(evaluate_expression('(-5+6*5)/5', macros, res))
        EXPECT_EQ(res, (-5+6*5)/5)

        EXPECT_TRUE(evaluate_expression('(-6+6*5)/5', macros, res))
        EXPECT_EQ(res, (-6+6*5)/5)

        EXPECT_TRUE(evaluate_expression('(+5+6*5)/7', macros, res))
        EXPECT_EQ(res, (+5+6*5)/7)

        EXPECT_TRUE(evaluate_expression('(+4+6*5)/7', macros, res))
        EXPECT_EQ(res, (+4+6*5)/7)

        EXPECT_TRUE(evaluate_expression('2**2', macros, res))
        EXPECT_EQ(res, 4)

    END_TEST

    block
        use fpx_parser
        use test_utils
        use fpx_path

        character(256), allocatable  :: files(:)
        character(:), allocatable :: ref
        integer :: i, j

        TEST_PRINT('fpx_unit_test')

#ifdef _FPM
        call chdir(join('tests','fpx'))
#else
        call chdir('fpx')
#endif
        call getfiles('', files)
        do i = 1, size(files)
            if (index(files(i), '.out') > 0) then
                cycle
            else if (index(files(i), '.ref') > 0) then
                cycle
            else if (index(files(i), '.f90') > 0 .or. index(files(i), '.F90') > 0 .or. index(files(i), '.c') > 0) then
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
#ifdef _FPM
        call chdir(join('..','..'))
#else
        call chdir('..')
#endif
    end block


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
#else
        call chdir('lfortran')
#endif
        call getfiles('', files)
        call add(global%macros, macro('__LFORTRAN__','1'))
        call add(global%macros, macro('__VERSION__'))
        call add(global%macros, macro('__LFORTRAN_MAJOR__'))
        call add(global%macros, macro('__LFORTRAN_MINOR__'))
        call add(global%macros, macro('__LFORTRAN_PATCHLEVEL__'))

        do i = 1, size(files)
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
#else
        call chdir('..')
#endif
    end block

    block
        use fpx_parser
        use test_utils
        use fpx_path
        use fpx_macro

        character(256), allocatable  :: files(:)
        character(:), allocatable :: runs, ref
        integer :: i, j

        TEST_PRINT('flang_unit_tests')

#ifdef _FPM
        call chdir(join('tests','flang'))
#else
        call chdir('flang')
#endif
        call getfiles('', files)

        call add(global%macros, macro('__flang_major__','20'))
        call add(global%macros, macro('__flang_minor__','1'))
        call add(global%macros, macro('__flang_patchlevel__','6'))
        !only for the tests
        call add(global%macros, macro('MACRO'))
        call add(global%macros, macro('FOO','1'))
        call add(global%macros, macro('BAR','2'))

        do i = 1, size(files)
            if (index(files(i), '.out') > 0) then
                cycle
            else if (index(files(i), '.ref') > 0) then
                cycle
            else if (index(files(i), '.f90') > 0 .or. &
                     index(files(i), '.F90') > 0) then
                call readruns(trim(files(i)), runs)
                if (len_trim(runs) > 0) then
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
            end if
        end do
        call clear(global%macros)
#ifdef _FPM
        call chdir(join('..','..'))
#else
        call chdir('..')
#endif
    end block

    block
        use test_utils
        use fpx_path

        character(256), allocatable :: lines(:)
        integer :: exitstat, cmdstat
#ifdef _FPM
#ifdef _WIN32
        character(*), parameter :: fpmcmd   = 'fpm run --runner > tests\output.txt'
        character(*), parameter :: fpxcmd   = ' tests\cli\main.f90 tests\cli\main.out.f90 -Itests\cli\include'
        character(*), parameter :: gfortcmd = 'gfortran -o tests\cli\main.out.exe tests\cli\main.out.f90'
        character(*), parameter :: execmd   = 'tests\cli\main.out.exe > nul 2>& 1'
        character(*), parameter :: outpath  = 'tests\output.txt'
#else
        character(*), parameter :: fpmcmd   = 'fpm run --runner > tests/output.txt'
        character(*), parameter :: fpxcmd   = ' tests/cli/main.f90 tests/cli/main.out.f90 -Itests/cli/include'
        character(*), parameter :: gfortcmd = 'gfortran -o tests/cli/main.out.exe tests/cli/main.out.f90'
        character(*), parameter :: execmd   = 'tests/cli/main.out.exe > /dev/null 2>& 1'
        character(*), parameter :: outpath  = 'tests/output.txt'
#endif
#else
        character(*), parameter :: fpmcmd   = 'fpm run --runner > output.txt 2>&1'
        character(*), parameter :: fpxcmd   = ' cli\main.f90 cli\main.out.f90 -Icli\include'
        character(*), parameter :: gfortcmd = 'gfortran -o cli\main.out.exe cli\main.out.f90'
        character(*), parameter :: execmd   = 'cli\main.out.exe > nul 2>&1'
        character(*), parameter :: outpath  = 'output.txt'
#endif
        call execute_command_line(fpmcmd)
        call getlines(outpath, lines, .false.)

        TEST('cli')
#ifdef _FPM
            call execute_command_line(trim(lines(1))//fpxcmd, exitstat = exitstat, cmdstat = cmdstat)
#else
            call execute_command_line(join('..', trim(lines(size(lines)-1))//fpxcmd), exitstat = exitstat, cmdstat = cmdstat)
#endif
            EXPECT_EQ(exitstat, 0)
            EXPECT_EQ(cmdstat, 0)
            call execute_command_line(gfortcmd, exitstat = exitstat, cmdstat = cmdstat)
            EXPECT_EQ(exitstat, 0)
            EXPECT_EQ(cmdstat, 0)
            print *, execmd
            call execute_command_line(execmd, exitstat = exitstat, cmdstat = cmdstat)
            EXPECT_EQ(exitstat, 0)
            EXPECT_EQ(cmdstat, 0)
        END_TEST

    end block

    block
        use fpx_parser
        use test_utils
        use fpx_path
        use fpx_macro

        character(256), allocatable  :: files(:)
        character(:), allocatable :: runs, ref
        integer :: i, j

        TEST_PRINT('c_unit_tests')

#ifdef _FPM
        call chdir(join('tests','c'))
#else
        call chdir('c')
#endif
        call getfiles('', files)

        do i = 1, size(files)
            if (index(files(i), '.out') > 0) then
                cycle
            else if (index(files(i), '.ref') > 0) then
                cycle
            else if (index(files(i), '.c') > 0) then
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
#else
        call chdir('..')
#endif
    end block
END_TESTPROGRAM