#define ADD(a,b) (a + b)
ADD(1,2)
// EXPECT: (1 + 2)

#define MIN(X,Y) ((X)<(Y)?(X):(Y))
MIN(5,10)
// EXPECT: ((5)<(10)?(5):(10))

// Variadic macros (C99)
#define DEBUG(fmt, ...) fprintf(stderr, fmt, __VA_ARGS__)
DEBUG("%s:%d\n", __FILE__, __LINE__);
// EXPECT: fprintf(stderr, "%s:%d\n", __FILE__, __LINE__);

// Zero arguments in variadic macro
#define INFO(x, ...) printf(x __VA_OPT__(, ) ##__VA_ARGS__)
INFO("hello");
// EXPECT: printf("hello");
INFO("hello %d", 42);
// EXPECT: printf("hello %d", 42);