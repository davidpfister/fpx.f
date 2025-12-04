#define A(x) x
#define B(x) A(x)
B(B)(42)
// EXPECT: 42

#define x 1
#define f(a) a
f(x)
// EXPECT: 1

#define x 2
f(x)
// EXPECT: 2   (macro replaced before argument substitution in C99+)

#define EMPTY
#define CALL(f) f(EMPTY)
#define PROBE ~, 1
CALL(PROBE)
// EXPECT: ~, 1