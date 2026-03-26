#include <assertion.inc>
TESTPROGRAM(test_logging)

    TEST('test_logging')
        use fpx_logging
        
        character(:), allocatable :: errmsg
        character(*), parameter :: nl = new_line('A')
        
        verbose = .false.
        nocolor = .true.
        !conditionals
        errmsg = render(diagnostic_report(LEVEL_ERROR, &
                message = 'Conditional nesting too deep', &
                source = __FILE__), &
                '#if x > 0', 1)
        
        EXPECT_STREQ(errmsg, 'error: Conditional nesting too deep'//nl//' --> '//__FILE__//':1:1-10'//nl//'  |'//nl//'1 | #if x > 0'//nl//'  | ^^^^^^^^^ '//nl//'  |')
    
        errmsg = render(diagnostic_report(LEVEL_ERROR, &
                message = 'Synthax error', &
                label = label_type('#elifdef without matching #if', 1, len_trim('#elifdef _DEBUG')), &
                source = __FILE__), &
                '#elifdef _DEBUG', 5)
                
        EXPECT_STREQ(errmsg, 'error: Synthax error'//nl//' --> '//__FILE__//':1:1-16'//nl//'  |'//nl//'5 | #elifdef _DEBUG'//nl//'  | ^^^^^^^^^^^^^^^ #elifdef without matching #if'//nl//'  |')
        
        !define
        errmsg = render(diagnostic_report(LEVEL_ERROR, &
                message = 'Synthax error', &
                label = label_type('Missing closing parenthesis in macro definition', len_trim('#define FOO(x, y') + 1, 1), &
                source = __FILE__), &
                '#define FOO(x, y', 12)
                
        EXPECT_STREQ(errmsg, 'error: Synthax error'//nl//'  --> '//__FILE__//':1:17-18'//nl//'   |'//nl//'12 | #define FOO(x, y'//nl//'   |                 ^ Missing closing parenthesis in macro definition'//nl//'   |')
        
        errmsg = render(diagnostic_report(LEVEL_ERROR, &
                    message = 'Reserved macro name', &
                    label = label_type('"defined" cannot be used as a macro name', 13, len('defined')), &
                    source = __FILE__), &
                    '#if defined(defined)', 3)
        EXPECT_STREQ(errmsg, 'error: Reserved macro name'//nl//' --> '//__FILE__//':1:13-20'//nl//'  |'//nl//'3 | #if defined(defined)'//nl//'  |             ^^^^^^^ "defined" cannot be used as a macro name'//nl//'  |')
        
        errmsg = render(diagnostic_report(LEVEL_WARNING, &
                        message = 'Unknown macro', &
                        label = label_type('TEST' // ' not found', index('#undef test', 'test'), len('test')), &
                        source = __FILE__), &
                        '#undef TEST')
        EXPECT_STREQ(errmsg, 'warning: Unknown macro'//nl//' --> '//__FILE__//':1:8-12'//nl//'  |'//nl//'1 | #undef TEST'//nl//'  |        ^^^^ TEST not found'//nl//'  |')
        
        !include
        errmsg = render(diagnostic_report(LEVEL_ERROR, &
                        message = 'Malformed #include directive', &
                        label = label_type('Filepath should either be delimited by "<...>" or "..."', index('#include myfile.inc', 'myfile.inc'), len('myfile.inc')), &
                        source = __FILE__), &
                        '#include myfile.inc', 5)
        EXPECT_STREQ(errmsg, 'error: Malformed #include directive'//nl//' --> '//__FILE__//':1:10-20'//nl//'  |'//nl//'5 | #include myfile.inc'//nl//'  |          ^^^^^^^^^^ Filepath should either be delimited by "<...>" or "..."'//nl//'  |')
    
        errmsg = render(diagnostic_report(LEVEL_ERROR, &
                        message = 'File not found', &
                        label = label_type('Cannot find include file myfile.inc', index('#include <myfile.inc>', 'myfile.inc'), len('myfile.inc')), &
                        source = __FILE__), &
                        '#include <myfile.inc>', 12)
        EXPECT_STREQ(errmsg, 'error: File not found'//nl//'  --> '//__FILE__//':1:11-21'//nl//'   |'//nl//'12 | #include <myfile.inc>'//nl//'   |           ^^^^^^^^^^ Cannot find include file myfile.inc'//nl//'   |')

    END_TEST
END_TESTPROGRAM