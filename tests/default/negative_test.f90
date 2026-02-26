module negative_test
implicit none

contains

subroutine test_negative()

#ifndef FEATURE_A
    print *, "FEATURE_A is NOT defined"
#elifndef FEATURE_B
    print *, "FEATURE_A defined, but FEATURE_B NOT defined"
#elifndef FEATURE_C
    print *, "FEATURE_A and FEATURE_B defined, but FEATURE_C NOT defined"
#else
    print *, "All features defined"
#endif

end subroutine test_negative

end module negative_test