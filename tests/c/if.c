#if 0
zero
#elif 1
one
#else
else
#endif
// EXPECT: one

#if defined(FOO)
yes
#else
no
#endif
// EXPECT: no

#define FOO
#if defined(FOO) && !defined(BAR)
ok
#endif
// EXPECT: ok

#if defined FOO
yes
#endif