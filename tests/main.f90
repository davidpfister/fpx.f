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
        use fpx_path

        character(256), allocatable  :: files(:)
        character(:), allocatable :: ref
        integer :: i, j
        
        TEST_PRINT('lfortran_unit_test')

#ifdef _FPM
        call chdir(join('tests','lfortran')
#else
        call chdir('lfortran')
#endif
        call getfiles('', files)
        global = global_t(5)
        call global%macros(1)%set('__LFORTRAN__','1')
        call global%macros(2)%set('__VERSION__')
        call global%macros(3)%set('__LFORTRAN_MAJOR__')
        call global%macros(4)%set('__LFORTRAN_MINOR__')
        call global%macros(5)%set('__LFORTRAN_PATCHLEVEL__')

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
#ifdef _FPM
        call chdir(join('..','..')
#else
        call chdir('..')
#endif
    end block
    
    block
        use fpx_parser
        use test_utils
        use fpx_path

        character(256), allocatable  :: files(:)
        character(:), allocatable :: runs, ref
        integer :: i, j
        
        TEST_PRINT('flang_unit_tests')

#ifdef _FPM
        call chdir(join('tests','flang')
#else
        call chdir('flang')
#endif
        call getfiles('', files)
        
        global = global_t(6)
        call global%macros(1)%set('__flang_major__','20')
        call global%macros(2)%set('__flang_minor__','1')
        call global%macros(3)%set('__flang_patchlevel__','6')
        !only for the tests
        call global%macros(4)%set('MACRO')
        call global%macros(5)%set('FOO','1')
        call global%macros(6)%set('BAR','2')

        do i = 1, size(files)
            if (index(files(i), '.out') > 0) then
                cycle
            else if (index(files(i), '.ref') > 0) then
                cycle
            else if (index(files(i), '.f90') > 0 .or. & 
                     index(files(i), '.F90') > 0 .or. &
                     index(files(i), '.F') > 0 .or.   & 
                     index(files(i), '.f') > 0) then
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
                        ref = trim(files(i)); ref(len(ref)-2:) = 'ref'
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
#ifdef _FPM
        call chdir(join('..','..')
#else
        call chdir('..')
#endif
    end block
END_TESTPROGRAM