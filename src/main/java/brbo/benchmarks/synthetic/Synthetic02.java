package brbo.benchmarks.synthetic;

import brbo.benchmarks.Common;

public abstract class Synthetic02 extends Common {
  void f(int n, int m) {
    if (n <= 0 || m <= 0)
      return;
    int R = 0;
    int i = 0;
    boundAssertion (R <= 2 * m * n);
    while (i < n) {
      int j = 0;
      while (j < m) {
        j++;
        R = R + 1;
      }
      j = 0;
      while (j < m) {
        j++;
        if (ndBool()) {
          R = R + 1;
        }
      }
      i++;
    }
  }
}