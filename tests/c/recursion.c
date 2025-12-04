#define X x
#define x X
X
// EXPECT: X        (infinite recursion must be detected and stopped)

#define RECURSE RECURSE
RECURSE
// Should either error or stop after reasonable depth