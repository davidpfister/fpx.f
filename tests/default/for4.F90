#define NUMERICS [integer, real]

#for T in NUMERICS

type(T) function zero_##T()

    zero_##T = 0

end function

#endfor