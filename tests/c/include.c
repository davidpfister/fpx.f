// test.c
#include "header.h"
MAGIC
// EXPECT: 123

#include "header2.h"
#include "header2.h"
x
// EXPECT: int x;     (only once)