module platform_test
implicit none

contains

subroutine test_platform()

#ifdef __LINUX__
    print *, "Compiled for Linux"
#elifdef __WINDOWS__
    print *, "Compiled for Windows"
#elifndef __MACOS__
    print *, "Not macOS"
#else
    print *, "Unknown platform"
#endif

end subroutine test_platform

end module platform_test