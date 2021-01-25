package brbo.benchmarks.synthetic;

import brbo.benchmarks.Common;

public abstract class Synthetic07 extends Common {
  void f(int n, int m, int l) {
    if (n <= 0 || m <= 0 || l <= 0)
      return;
    int R = 0;
    int i = 0;
    int j = 0;
    int k = 0;
    boundAssertion (R <= 2 * l * m * n);
    while (i < n) {
      j = 0;
      while (j < m) {
        k = 0;
        while (k < l) {
          R = R + 1;
          k++;
        }
        j++;
      }
      i++;
    }

    i = 0;
    while (i < n) {
      j = 0;
      while (j < m) {
        k = 0;
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