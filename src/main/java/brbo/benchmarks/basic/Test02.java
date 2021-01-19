package brbo.benchmarks.basic;

class Test02 {
  void f(int n, int m) {
    int R = 0;
    int i = 0;
    assert (!(n >= 0 && m >= 0) || (R <= m + n));
    while (i < n) {
      i++;
      R = R + 1;
    }

    i = 0;
    while (i < m) {
      i++;
      R = R + 1;
    }
  }
}