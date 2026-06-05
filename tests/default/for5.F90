#for T in [integer]
type(T) :: x
#endfor

#define NUMERICS [integer, real]
#define CONCAT(a, b) a##b

#for T in NUMERICS
#for I in [1,2]
type(T) :: CONCAT(value_,I)
#endfor
#endfor