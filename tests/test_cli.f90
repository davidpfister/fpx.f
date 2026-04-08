#include <assertion.inc>
TESTPROGRAM(test_cli)
    TEST('cli')
        use test_utils
        use fpx_path

        character(256), allocatable :: lines(:)
        integer :: exitstat, cmdstat
#ifdef _FPM
#ifdef _WIN32
        character(*), parameter :: fpmcmd   = 'fpm run --runner > tests\output.txt'
        character(*), parameter :: fpxcmd   = ' tests\cli\test.f90 tests\cli\test.out.f90 -Itests\cli\include'
        character(*), parameter :: exepath  = 'tests\cli\test.out.exe'
        character(*), parameter :: gfortcmd = 'gfortran -o '//exepath//' tests\cli\test.out.f90'
        character(*), parameter :: execmd   =  exepath//' > nul 2>& 1'
        character(*), parameter :: outpath  = 'tests\output.txt'
#else
        character(*), parameter :: fpmcmd   = 'fpm run --runner > tests/output.txt'
        character(*), parameter :: fpxcmd   = ' tests/cli/test.f90 tests/cli/test.out.f90 -Itests/cli/include'
        character(*), parameter :: exepath  = 'tests/cli/test.out.exe'
        character(*), parameter :: gfortcmd = 'gfortran -o '//exepath//' tests/cli/test.out.f90'
        character(*), parameter :: execmd   =  exepath//' > /dev/null 2>& 1'
        character(*), parameter :: outpath  = 'tests/output.txt'
#endif
#else
        character(*), parameter :: fpmcmd   = 'fpm run --runner > output.txt 2>&1'
        character(*), parameter :: fpxcmd   = ' cli\test.f90 cli\test.out.f90 -Icli\include'
        character(*), parameter :: exepath  = 'cli\test.out.exe'
        character(*), parameter :: gfortcmd = 'gfortran -o '//exepath//' cli\test.out.f90'
        character(*), parameter :: execmd   =  exepath//' > nul 2>&1'
        character(*), parameter :: outpath  = 'output.txt'
#endif
#ifndef _FPM
        call chdir('..')
#endif
        call execute_command_line(fpmcmd)
        call getlines(outpath, lines, keepall = .false., delete = .true.)
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

        open(unit = 1234, iostat=exitstat, file=exepath,status='old')
        if(exitstat == 0) close(1234, status='delete')
    END_TEST
END_TESTPROGRAM