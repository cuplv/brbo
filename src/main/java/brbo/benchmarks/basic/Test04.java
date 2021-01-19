package brbo.benchmarks.basic;

class Test04 {
  void f(int n, int m, int l) {
    int R = 0;
    int i = 0;
    assert (!(n >= 0 && m >= 0 && l >= 0) || (R <= n * m + l));
    while (i < n) {
      int j = 0;
      while (j < m) {
        j++;
        R++;
      }
      i++;
    }

    for (int k = 0; k < l; k++) {
      R++;
    }
  }
}