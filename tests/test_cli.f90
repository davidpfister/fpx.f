#include <assertion.inc>
TESTPROGRAM(test)
    TEST('cli')
        use test_utils
        use fpx_path

        character(256), allocatable :: lines(:)
        integer :: exitstat, cmdstat
#ifdef _FPM
#ifdef _WIN32
        character(*), parameter :: fpmcmd   = 'fpm run --runner > tests\output.txt'
        character(*), parameter :: fpxcmd   = ' tests\cli\test.f90 tests\cli\test.out.f90 -Itests\cli\include'
        character(*), parameter :: gfortcmd = 'gfortran -o tests\cli\test.out.exe tests\cli\test.out.f90'
        character(*), parameter :: execmd   = 'tests\cli\test.out.exe > nul 2>& 1'
        character(*), parameter :: outpath  = 'tests\output.txt'
#else
        character(*), parameter :: fpmcmd   = 'fpm run --runner > tests/output.txt'
        character(*), parameter :: fpxcmd   = ' tests/cli/test.f90 tests/cli/test.out.f90 -Itests/cli/include'
        character(*), parameter :: gfortcmd = 'gfortran -o tests/cli/test.out.exe tests/cli/test.out.f90'
        character(*), parameter :: execmd   = 'tests/cli/test.out.exe > /dev/null 2>& 1'
        character(*), parameter :: outpath  = 'tests/output.txt'
#endif
#else
        character(*), parameter :: fpmcmd   = 'fpm run --runner > output.txt 2>&1'
        character(*), parameter :: fpxcmd   = ' cli\test.f90 cli\test.out.f90 -Icli\include'
        character(*), parameter :: gfortcmd = 'gfortran -o cli\test.out.exe cli\test.out.f90'
        character(*), parameter :: execmd   = 'cli\test.out.exe > nul 2>&1'
        character(*), parameter :: outpath  = 'output.txt'
#endif
#ifndef _FPM
        call chdir('..')
#endif
        call execute_command_line(fpmcmd)
        call getlines(outpath, lines, .false.)
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
        call execute_command_line(execmd, exitstat = exitstat, cmdstat = cmdstat)
        EXPECT_EQ(exitstat, 0)
        EXPECT_EQ(cmdstat, 0)
    END_TEST
END_TESTPROGRAM