package brbo.benchmarks.synthetic;

import brbo.benchmarks.Common;

abstract class Synthetic09 extends Common {
  void f(int n, int m) {
    if (n <= 0 || m <= 0)
      return;
    int R = 0;
    int i = 0;
    mostPreciseBound(R <= 2 * m * n);
    lessPreciseBound(R <= 3 * m * n);
    while (i < n) {
      int j = 0;
      while (j < m) {
        j++;
        if (ndBool()) {
          R = R + 1;
        }
        else {
          R = R + 1;
        }
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