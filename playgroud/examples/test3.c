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

void main(int n, int a, int b)
{
  int r = 0;
  int r1 = 0;
  int r1_star = 0;
  int r1_sharp = -1;
  assume(n >= 0);
  int c1 = 0;
  int c2 = 0;
  for (int i = 0; i < n; i++) {
    int e = 0;
    if (i < 2) {
      e = a;
      c1++;
    }
    else {
      e = b;
      c2++;
    }
    r += e;

    r1_star = r1 > r1_star ? r1 : r1_star;
    r1 = 0;
    r1_sharp++;
    r1 += e;
    assert (r1_sharp * r1_star + r1 <= n * a || r1_sharp * r1_star + r1 <= (n - 2) * b + 2 * a);
  }
  // assert (r <= n * a || r <= (n - 1) * b + 1 * a);
  // assert (c1 <= 3);
  // assert (n <= 3 || c2 <= n - 3);
}