#define NUMERICS [integer, real]
#define KINDS [4,8]
#define CONCAT(a, b) a##b

#for T in NUMERICS
#for K in KINDS

type(T(K)) :: CONCAT(CONCAT(var_,T),K)

#endfor
#endfor