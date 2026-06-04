#for T in [integer, real, complex]

function add_##T(a,b) result(c)
    T :: a,b,c
    c = a+b
end function

#endfor