module feature_test
implicit none

contains

subroutine test_features()

#ifdef FEATURE_A
    print *, "FEATURE_A is enabled"
#elifdef FEATURE_B
    print *, "FEATURE_B is enabled"
#elifdef FEATURE_C
    print *, "FEATURE_C is enabled"
#else
    print *, "No feature enabled"
#endif

end subroutine test_features

end module feature_test