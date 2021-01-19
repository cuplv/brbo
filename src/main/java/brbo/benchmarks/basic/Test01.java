package brbo.benchmarks.basic;

class Test01 {
  void f(int n) {
    int R = 0;
    assert (n < 0 || R <= n);
    int i = 0;
    while (i < n) {
      i++;
      R = R + 1;
    }
  }
}