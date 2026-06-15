#define CONCAT(a, b) a##b
#for T in [integer,real,complex]
#for K in [32,64]
#for A in [cpu,gpu]
type(T) :: ${A}_${K}
#endfor
integer, public :: i_${T}_${K}
#endfor

type, public :: object_${T}
end type
#endfor