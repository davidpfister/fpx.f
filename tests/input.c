#define STR(x) \
"test" \\
#x
#define CONCAT(a, b) a ## b
#define PRINT(...) printf(__VA_ARGS__)
int main() {
  STR(hello);
  CONCAT(var, 123) = 42;
  PRINT("Hello, %d", 42);
}