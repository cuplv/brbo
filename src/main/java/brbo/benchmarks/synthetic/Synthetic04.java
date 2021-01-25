package brbo.benchmarks.synthetic;

import brbo.benchmarks.Common;

public abstract class Synthetic04 extends Common {
  void f(int n, int m, int l) {
    if (n <= 0 || m <= 0 || l <= 0)
      return;
    int R = 0;
    int i = 0;
    boundAssertion (R <= l * m * n);
    while (i < n) {
      int j = 0;
      while (j < m) {
        int k = 0;
        while (k < l) {
          R = R + 1;
          k++;
        }
        j++;
      }
      i++;
    }
  }
}