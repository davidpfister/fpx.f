module value_test
implicit none

contains

subroutine test_value()

#ifdef VALUE
#if VALUE == 10
    print *, "VALUE is 10"
#elif VALUE == 20
    print *, "VALUE is 20"
#else
    print *, "VALUE has another value"
#endif
#else
    print *, "VALUE not defined"
#endif

end subroutine test_value

end module value_test