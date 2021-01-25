package brbo.benchmarks.synthetic;

import brbo.benchmarks.Common;

public abstract class Synthetic05 extends Common {
  void f(int n, int m, int l, int o) {
    if (n <= 0 || m <= 0 || l <= 0 || o <= 0)
      return;
    int R = 0;
    int i = 0;
    boundAssertion (R <= l * m * n * o);
    while (i < n) {
      int j = 0;
      while (j < m) {
        int k = 0;
        while (k < l) {
          int a = 0;
          while (a < o) {
            a++;
            R = R + 1;
          }
          k++;
        }
        j++;
      }
      i++;
    }
  }
}