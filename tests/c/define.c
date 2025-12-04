#define defined(x) 0
#if defined(defined)
bad
#else
good
#endif

#if defined defined(FOO)