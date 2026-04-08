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

        !macro
        errmsg = render(diagnostic_report(LEVEL_ERROR, &
                        message='Variadic macro issue', &
                        label=label_type('Too few arguments for macro FOO', index('FOO(a, b)','(') + 1, index('FOO(a, b)',')') - index('FOO(a, b)','(') - 1), &
                        source=__FILE__), &
                        'FOO(a, b)', 1)
        EXPECT_STREQ(errmsg, 'error: Variadic macro issue'//nl//' --> '//__FILE__//':1:5-9'//nl//'  |'//nl//'1 | FOO(a, b)'//nl//'  |     ^^^^ Too few arguments for macro FOO'//nl//'  |')
        
        errmsg = render(diagnostic_report(LEVEL_ERROR, &
                        message='Function-like macro issue', &
                        label=label_type('Too few arguments for macro FOO', index('FOO(a, b)','(') + 1, index('FOO(a, b)',')') - index('FOO(a, b)','(') - 1), &
                        source=__FILE__), &
                        'FOO(a, b)', 1)
        EXPECT_STREQ(errmsg, 'error: Function-like macro issue'//nl//' --> '//__FILE__//':1:5-9'//nl//'  |'//nl//'1 | FOO(a, b)'//nl//'  |     ^^^^ Too few arguments for macro FOO'//nl//'  |')
        
        errmsg = render(diagnostic_report(LEVEL_ERROR, &
                        message='Synthax error', &
                        label=label_type('No token before ##', 1, 2), &
                        source=__FILE__), &
                        '## TEST', 3)
        EXPECT_STREQ(errmsg, 'error: Synthax error'//nl//' --> '//__FILE__//':1:1-3'//nl//'  |'//nl//'3 | ## TEST'//nl//'  | ^^ No token before ##'//nl//'  |')
        
        errmsg = render(diagnostic_report(LEVEL_ERROR, &
                        message='Failed macro expansion', &
                        label=label_type('Circular macro detected', index('#define FOO FOO', 'FOO'), len('FOO')), &
                        source=__FILE__), &
                        '#define FOO FOO', 3)
        EXPECT_STREQ(errmsg, 'error: Failed macro expansion'//nl//' --> '//__FILE__//':1:9-12'//nl//'  |'//nl//'3 | #define FOO FOO'//nl//'  |         ^^^ Circular macro detected'//nl//'  |')
        
        !operator
        
        !parser
        errmsg = render(diagnostic_report(LEVEL_ERROR, &
                        message='Error opening input file: ' // __FILE__, &
                        source=__FILE__), &
                        '')
        EXPECT_STREQ(errmsg, 'error: Error opening input file: '//__FILE__//nl//' --> '//__FILE__//':1:1-1'//nl//'  |'//nl//'  |')
        
        errmsg = render(diagnostic_report(LEVEL_ERROR, &
                        message='Unclosed conditional block at end of file', &
                        label=label_type('Missing conditional statement #endif', 1, 1), &
                        source=__FILE__), &
                        'subroutine foo(x)', 1)
        EXPECT_STREQ(errmsg, 'error: Unclosed conditional block at end of file'//nl//' --> '//__FILE__//':1:1-2'//nl//'  |'//nl//'1 | subroutine foo(x)'//nl//'  | ^ Missing conditional statement #endif'//nl//'  |')
        
        errmsg = render(diagnostic_report(LEVEL_ERROR, &
                        message='Unexpected character', &
                        label=label_type('Trailing new line "\"', len('#define _DEBUG \'), 1), &
                        source=__FILE__), &
                        '#define _DEBUG \', 1)
        EXPECT_STREQ(errmsg, 'error: Unexpected character'//nl//' --> '//__FILE__//':1:16-17'//nl//'  |'//nl//'1 | #define _DEBUG '//char(92)//nl//'  |                ^ Trailing new line "\"'//nl//'  |')

        !token
        errmsg = render(diagnostic_report(LEVEL_ERROR, &
                        message='The maximum number of tokens has been reached', &
                        label=label_type('Too many tokens in expression.', 1, 1)), &
                        'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.')
        EXPECT_STREQ(errmsg, 'error: The maximum number of tokens has been reached'//nl//'  |'//nl//'1 | Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.'//nl//'  | ^ Too many tokens in expression.'//nl//'  |')
    END_TEST
END_TESTPROGRAM