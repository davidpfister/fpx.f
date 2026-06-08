#define NUMERICS [integer, real]
#define CONCAT(a, b) a##b

#for T in NUMERICS

type(T) function CONCAT(zero_,T)()

    CONCAT(zero_,T) = 0

end function

#endfor