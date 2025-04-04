#define VERSION 2
#define INC(x) x + 1
#define ADD(a, b) INC(a) + b
int main() {
  int x = ADD(2, 3);
  return 0;
}