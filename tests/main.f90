#include <assertion.inc>
TESTPROGRAM(main)

    TEST('test_expression')
        use fpx_token
        use fpx_macro
        
        type(macro_t) :: macros(0)
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

        character(256), allocatable  :: files(:)
        character(:), allocatable :: ref
        integer :: i, j
        
        TEST_PRINT('lfortran_unit_test')

#ifdef _FPM
        call getfiles('tests\lfortran', files)
#else
        call getfiles('lfortran', files)
#endif

        global = global_t(5)
        global%macros(1)%name = '__LFORTRAN__'; global%macros(1)%value = '1'
        global%macros(2)%name = '__VERSION__'
        global%macros(3)%name = '__LFORTRAN_MAJOR__'
        global%macros(4)%name = '__LFORTRAN_MINOR__'
        global%macros(5)%name = '__LFORTRAN_PATCHLEVEL__'

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
                    ref = trim(files(i)); ref(len(ref)-2:) = 'ref'
!#ifdef _WIN32
!                   call execute_command_line('fc "'//trim(files(i))//'.out" "'//ref//'" > nul 2>&1', exitstat = ierr)
!#else
!                   call execute_command_line("cmp -s '"//trim(files(i))//".out' '"//ref//"'", exitstat = ierr)
!#endif
                    call getlines(trim(files(i))//'.out', actual, .false.)
                    call getlines(ref, expected, .false.)
                    
                    EXPECT_EQ(size(actual), size(expected))
                    
                    do j = 1, min(size(actual), size(expected))
                        EXPECT_STREQ(trim(actual(j)), trim(expected(j)))
                    end do
                END_TEST
            end if
        end do
    end block
    
    block
        use fpx_parser
        use test_utils

        character(256), allocatable  :: files(:)
        character(256), allocatable, target  :: checks(:)
        character(:), allocatable :: runs
        integer :: i, j
        
        TEST_PRINT('flang_unit_tests')

#ifdef _FPM
        call getfiles('tests\flang', files)
#else
        call getfiles('flang', files)
#endif
        do i = 1, size(files)
            call getruns(trim(files(i)), runs)
            if (len_trim(runs) > 0) then
                TEST_PRINT(filename(files(i)))
                call getchecks(trim(files(i)), checks)
                call preprocess(trim(files(i)), trim(files(i))//'.out')
                TEST(filename(files(i)))
                    logical :: exists
                    integer :: ierr, unit
                    character(:), allocatable :: line
                    character(256), pointer :: check => null() 
                
                    inquire(file=trim(files(i))//'.out', exist=exists)
                    ASSERT_TRUE(exists)
        
                    open (newunit=unit, file=trim(files(i))//'.out', status='old', action='read', iostat=ierr)
                    ASSERT_TRUE(ierr /= 0)
        
                    j = 1
                    do while (.true.)
                        call readline(unit, line, ierr)
                        if (ierr /= 0 .and. .not. is_iostat_end(ierr)) stop -1
                    
                        if (j > size(checks)) exit
                        check => checks(j)
                        EXPECT_STRCASEEQ(line, check)
                        j = j + 1
                        nullify(check)
                    end do
                    close(unit)
                END_TEST
            end if
        end do
    end block
END_TESTPROGRAM