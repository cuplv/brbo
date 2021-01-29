package brbo.benchmarks.synthetic;

import brbo.benchmarks.Common;

abstract class Synthetic11 extends Common {
  void f(int n, int m) {
    if (n <= 0 || m <= 0)
      return;
    int R = 0;
    mostPreciseBound(R <= 1);
    while (true) {
      R = R + 1;
      R = R - 1;
    }
  }
}