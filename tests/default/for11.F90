#define CONCAT(a, b) a##b
#for T in [integer,real,complex]
#for K in [32,64]
#for A in [cpu,gpu]
type(T) :: CONCAT(CONCAT(A,_),K)
#endfor
integer, public :: CONCAT(CONCAT(i_, T), K)
#endfor

type, public :: CONCAT(object_, T)
end type
#endfor