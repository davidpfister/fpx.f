#define TYPES [integer, real]
#define NUMERICS TYPES

#for T in NUMERICS
type(T) :: x
#endfor