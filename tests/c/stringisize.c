#define STR(x) #x
STR(hello world)
// EXPECT: "hello world"

#define FOO 123
STR(FOO)
// EXPECT: "FOO"        (not "123" — stringizing before replacement)