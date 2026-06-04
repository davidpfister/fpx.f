#define NUMERICS [integer, real]
#define KINDS [4,8]

#for T in NUMERICS
#for K in KINDS

type(T(K)) :: var_##T##K

#endfor
#endfor