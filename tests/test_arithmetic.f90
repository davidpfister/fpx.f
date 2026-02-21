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
END_TESTPROGRAM
