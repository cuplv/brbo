package brbo.benchmarks.synthetic;

class Synthetic01 {
  void f(int n, int m) {
    int R = 0;
    int i = 0;
    assert (!(n >= 0 && m >= 0) || (R <= m * n));
    while (i < n) {
      int j = 0;
      while (j < m) {
        j++;
        R = R + 1;
      }
      i++;
    }
  }
}