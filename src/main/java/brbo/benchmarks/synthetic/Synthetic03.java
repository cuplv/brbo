package brbo.benchmarks.synthetic;

class Synthetic03 {
  void f(int n, int m) {
    int R = 0;
    int i = 0;
    assert (!(n >= 0 && m >= 0) || (R <= 2 * m * n));
    while (i < n) {
      int j = 0;
      while (j < m) {
        j++;
        R = R + 1;
      }
      i++;
    }

    i = 0;
    while (i < n) {
      int k = 0;
      while (k < m) {
        k++;
        R = R + 1;
      }
      i++;
    }
  }
}