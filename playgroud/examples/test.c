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

void main(int text)
  {
    int C16 = 0;
    int C19 = 0;
    int D0 = 0;
    int D0p = 0;
    int D1 = 0;
    int D1p = 0;
    if (text <= 0)
    {
      return;
    }

    int R = 0;
    // mostPreciseBound(R <= text);
    // lessPreciseBound(R <= MAX * text + MAX);
    int linePointer = 0;
    int startTagLocation = 0;
    int endTagLocation = 0;
    int stringBuilder = 0;
    D0p = D0;
    D0 = 0;
    C16 = C16 + 1;
    D1p = D1;
    D1 = 0;
    C19 = C19 + 1;
    while (endTagLocation < text)
    {
      startTagLocation = ndInt2(endTagLocation + 1, text);
      endTagLocation = ndInt2(startTagLocation + 1, text);
      stringBuilder += startTagLocation - linePointer;
      D0 = D0 + (startTagLocation - linePointer);
      assert (D0 <= text);
      // assert(D0 + D1 <= text);
      R = R + (startTagLocation - linePointer);
      linePointer = endTagLocation;
    }
    stringBuilder += text - linePointer;
    
    D1 = D1 + (text - linePointer);
    assert(D1 <= text - 2);
    R = R + (text - linePointer);
    // assert (R <= text - 1);
  }