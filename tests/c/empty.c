#define PASTE(a,b) a ## b
#define EMPTY
PASTE(EMPTY,123)
// EXPECT: 123
PASTE(123,EMPTY)
// EXPECT: 123
PASTE(EMPTY,EMPTY)
// EXPECT: nothing (placemarker removed)