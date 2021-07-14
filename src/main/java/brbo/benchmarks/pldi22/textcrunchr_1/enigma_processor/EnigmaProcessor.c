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

void main(int inps) {
    if (inps < 0) {
        return;
    }

    int R = 0;
    int sb = 0;
    int it = inps;
    while (it > 0) {
        int line = ndInt2(1, it);
        it -= line;
        sb += line;
        R += line;
        assert (R <= inps);
    }
    // assert (R <= inps);

    int theString = sb;
    R += sb;
    assert (R <= inps * 2);

    // EnigmaMachine machine = EnigmaFactory.buildEnigmaMachine();
    R += 52;
    R += 52;
    R += 52;
    R += 26;
    assert (R <= inps * 2 + 182);

    int sb2 = 0;
    for (int i = 0; i < theString; i++) {
        if (ndBool()) {
            ;
        }
        sb2 += 1;
        R += 1;
        assert (R <= inps * 3 + 182);
    }
    // assert (R <= inps * 3 + 182);

    int name = 32; // String name = "Enigma transformation (5, 9, 14)";
    int value = sb2; // String value = encodedString;

    // Deallocations
    R -= sb;
    assert (R <= inps * 2 + 182);
    R -= theString;
    assert (R <= inps + 182);
}