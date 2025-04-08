#define VERSION 2
#define DEBUG 0
#if VERSION > 1 && DEBUG
  int feature = 1;
#else
  int feature = 0;
#endif

#if !(VERSION == 2) || defined(UNDEF)
  int test = 0;
#else
  int test = 1;
#endif

#if (VERSION + 1) > 2
  int advanced = 1;
#else
  int advanced = 0;
#endif