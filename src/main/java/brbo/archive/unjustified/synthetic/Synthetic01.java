package brbo.archive.unjustified.synthetic;

import brbo.benchmarks.Common;

abstract class Synthetic01 extends Common {
  void f(int n, int m) {
    if (n <= 0 || m <= 0)
      return;
    int R = 0;
    int i = 0;
    mostPreciseBound(R <= 2 * m * n);
    while (i < n) {
      int j = 0;
      while (j < m) {
        j++;
        R = R + 1;
      }
      j = 0;
      while (j < m) {
        j++;
        R = R + 1;
      }
      i++;
    }
  }
}