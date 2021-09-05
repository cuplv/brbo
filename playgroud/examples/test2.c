extern void __VERIFIER_error() __attribute__((noreturn));
extern void __VERIFIER_assume (int);
extern int __VERIFIER_nondet_int ();
#define static_assert __VERIFIER_assert
#define assume __VERIFIER_assume
#define LARGE_INT 1000000
#define true 1
#define false 0
#define boolean int
#define MAX 8
void __VERIFIER_assert(int cond) {
  if (!(cond)) {
    ERROR: __VERIFIER_error();
  }
  return;
}
void assert(int cond) {
  if (!(cond)) {
    ERROR: __VERIFIER_error();
  }
  return;
}
int ndInt() {
  return __VERIFIER_nondet_int();
}
int ndBool() {
  int x = ndInt();
  assume(x == 1 || x == 0);
  return x;
}
int ndInt2(int lower, int upper) {
  int x = ndInt();
  assume(lower <= x && x <= upper);
  return x;
}

void main(int n)
  {
    assume(n > 0);
    int R = 0;
    int i = 0;
    while (i < n) {
      int j = 0;
      while (j < n) {
        R++;
        j++;
        assert (R <= n * n);
      }
      i++;
    }
  }