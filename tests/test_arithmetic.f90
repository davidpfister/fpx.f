#include <assertion.inc>
TESTPROGRAM(main)

    TEST('test_expression')
        use fpx_operators
        use fpx_macro

        type(macro), allocatable :: macros(:)
        integer :: res

        allocate(macros(0))

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

        EXPECT_TRUE(evaluate_expression('1 ? 2 : 3', macros, res))
        EXPECT_EQ(res, 2)

        EXPECT_TRUE(evaluate_expression('0 ? 2 : 3', macros, res))
        EXPECT_EQ(res, 3)

        EXPECT_TRUE(evaluate_expression('5 ? 10 : 20', macros, res))
        EXPECT_EQ(res, 10)

        EXPECT_TRUE(evaluate_expression('-1 ? 10 : 20', macros, res))
        EXPECT_EQ(res, 10)

        EXPECT_TRUE(evaluate_expression('0 ? 10 : 20', macros, res))
        EXPECT_EQ(res, 20)

        EXPECT_TRUE(evaluate_expression('1 == 1 ? 100 : 200', macros, res))
        EXPECT_EQ(res, 100)

        EXPECT_TRUE(evaluate_expression('1 == 0 ? 100 : 200', macros, res))
        EXPECT_EQ(res, 200)

        EXPECT_TRUE(evaluate_expression('5 > 3 ? 7 : 9', macros, res))
        EXPECT_EQ(res, 7)
        EXPECT_TRUE(evaluate_expression('5 < 3 ? 7 : 9', macros, res))
        EXPECT_EQ(res, 9)

        EXPECT_TRUE(evaluate_expression('0 || 1 ? 11 : 22', macros, res))
        EXPECT_EQ(res, 11)

        EXPECT_TRUE(evaluate_expression('0 && 1 ? 11 : 22', macros, res))
        EXPECT_EQ(res, 22)

        EXPECT_TRUE(evaluate_expression('0 || 1 ? 2 : 3', macros, res))
        EXPECT_EQ(res, 2)

        EXPECT_TRUE(evaluate_expression('0 || 0 ? 2 : 3', macros, res))
        EXPECT_EQ(res, 3)

        EXPECT_TRUE(evaluate_expression('1 && 0 ? 2 : 3', macros, res))
        EXPECT_EQ(res, 3)

        EXPECT_TRUE(evaluate_expression('1 && 1 ? 2 : 3', macros, res))
        EXPECT_EQ(res, 2)

        EXPECT_TRUE(evaluate_expression('1 ? 2+3 : 4+5', macros, res))
        EXPECT_EQ(res, 5)

        EXPECT_TRUE(evaluate_expression('0 ? 2+3 : 4+5', macros, res))
        EXPECT_EQ(res, 9)

        EXPECT_TRUE(evaluate_expression('1 ? 2*3 : 4*5', macros, res))
        EXPECT_EQ(res, 6)

        EXPECT_TRUE(evaluate_expression('0 ? 2*3 : 4*5', macros, res))
        EXPECT_EQ(res, 20)

        EXPECT_TRUE(evaluate_expression('1 ? 2 : 3 ? 4 : 5', macros, res))
        EXPECT_EQ(res, 2)

        EXPECT_TRUE(evaluate_expression('0 ? 2 : 3 ? 4 : 5', macros, res))
        EXPECT_EQ(res, 4)

        EXPECT_TRUE(evaluate_expression('0 ? 2 : 0 ? 4 : 5', macros, res))
        EXPECT_EQ(res, 5)

        EXPECT_TRUE(evaluate_expression('1 ? (0 ? 3 : 4) : 5', macros, res))
        EXPECT_EQ(res, 4)

        EXPECT_TRUE(evaluate_expression('0 ? (0 ? 3 : 4) : 5', macros, res))
        EXPECT_EQ(res, 5)

        EXPECT_TRUE(evaluate_expression('(1 ? 0 : 1) ? 7 : 8', macros, res))
        EXPECT_EQ(res, 8)

        EXPECT_TRUE(evaluate_expression('1 ? 2 : 3 ? 4 : 5 ? 6 : 7', macros, res))
        EXPECT_EQ(res, 2)

        EXPECT_TRUE(evaluate_expression('0 ? 2 : 3 ? 4 : 5 ? 6 : 7', macros, res))
        EXPECT_EQ(res, 4)

        EXPECT_TRUE(evaluate_expression('0 ? 2 : 0 ? 4 : 5 ? 6 : 7', macros, res))
        EXPECT_EQ(res, 6)

        EXPECT_TRUE(evaluate_expression('0 ? 2 : 0 ? 4 : 0 ? 6 : 7', macros, res))
        EXPECT_EQ(res, 7)

        EXPECT_TRUE(evaluate_expression('1 + (0 ? 2 : 3)', macros, res))
        EXPECT_EQ(res, 4)

        EXPECT_TRUE(evaluate_expression('(1 ? 2 : 3) + 4', macros, res))
        EXPECT_EQ(res, 6)

        EXPECT_TRUE(evaluate_expression('(0 ? 2 : 3) * 4', macros, res))
        EXPECT_EQ(res, 12)

        EXPECT_TRUE(evaluate_expression('10 / (1 ? 2 : 5)', macros, res))
        EXPECT_EQ(res, 5)

        EXPECT_TRUE(evaluate_expression('!0 ? 1 : 2', macros, res))
        EXPECT_EQ(res, 1)

        EXPECT_TRUE(evaluate_expression('!1 ? 1 : 2', macros, res))
        EXPECT_EQ(res, 2)

        EXPECT_TRUE(evaluate_expression('-1 ? 3 : 4', macros, res))
        EXPECT_EQ(res, 3)

        EXPECT_TRUE(evaluate_expression('(1 & 1) ? 10 : 20', macros, res))
        EXPECT_EQ(res, 10)

        EXPECT_TRUE(evaluate_expression('(1 & 0) ? 10 : 20', macros, res))
        EXPECT_EQ(res, 20)

        EXPECT_TRUE(evaluate_expression('(4 >> 2) ? 10 : 20', macros, res))
        EXPECT_EQ(res, 10)

        EXPECT_TRUE(evaluate_expression('(0 >> 2) ? 10 : 20', macros, res))
        EXPECT_EQ(res, 20)

        macros = [macro('A', '1'), &
                  macro('B', '0'), &
                  macro('C', '42')]

        EXPECT_TRUE(evaluate_expression('A ? 2 : 3', macros, res))
        EXPECT_EQ(res, 2)

        EXPECT_TRUE(evaluate_expression('B ? 2 : 3', macros, res))
        EXPECT_EQ(res, 3)

        EXPECT_TRUE(evaluate_expression('A ? C : 3', macros, res))
        EXPECT_EQ(res, 42)

        EXPECT_TRUE(evaluate_expression('B ? C : 3', macros, res))
        EXPECT_EQ(res, 3)

        EXPECT_TRUE(evaluate_expression('defined(A) ? 10 : 20', macros, res))
        EXPECT_EQ(res, 10)

        EXPECT_TRUE(evaluate_expression('defined(Z) ? 10 : 20', macros, res))
        EXPECT_EQ(res, 20)

        EXPECT_TRUE(evaluate_expression('defined(A) && A ? 100 : 200', macros, res))
        EXPECT_EQ(res, 100)

        EXPECT_TRUE(evaluate_expression('defined(A) && B ? 100 : 200', macros, res))
        EXPECT_EQ(res, 200)

        EXPECT_TRUE(evaluate_expression('defined(Z) ? 100 : 200', macros, res))
        EXPECT_EQ(res, 200)

        EXPECT_TRUE(evaluate_expression('defined(A) ? (B ? 1 : 2) : 3', macros, res))
        EXPECT_EQ(res, 2)

        EXPECT_TRUE(evaluate_expression('(1 ? 2 : 3)', macros, res))
        EXPECT_EQ(res, 2)

        EXPECT_TRUE(evaluate_expression('(0 ? 2 : 3)', macros, res))
        EXPECT_EQ(res, 3)

        EXPECT_TRUE(evaluate_expression('((1 ? 2 : 3))', macros, res))
        EXPECT_EQ(res, 2)

    END_TEST
END_TESTPROGRAM
