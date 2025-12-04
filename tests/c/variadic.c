#define LOG(...) printf(__VA_OPT__(__VA_ARGS__,) "empty")
LOG();
LOG("hello");
// EXPECT first: printf( "empty")
// EXPECT second: printf("hello", "empty")