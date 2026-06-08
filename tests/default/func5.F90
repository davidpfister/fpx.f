function outer() result(res)
    print *, __FUNC__
contains

    subroutine inner()
        print *, __FUNC__
    end subroutine

end function