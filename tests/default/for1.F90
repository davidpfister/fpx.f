#define CONCAT(a, b) a##b
#for T in [integer, real, complex]

function CONCAT(add_,T)(a,b) result(c)
    T :: a,b,c
    c = a+b
end function

#endfor