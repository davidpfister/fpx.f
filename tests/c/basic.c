// TEST: Simple object-like macro
#define FOO 42
FOO
// EXPECT: 42

#define EMPTY
EMPTY X
// EXPECT:  X

#define GLUE(a,b) a ## b
GLUE(12,34)
// EXPECT: 1234

#define HELLO "Hello" " " "World"
HELLO
// EXPECT: "Hello" " " "World"     (string concatenation happens later)