package brbo.benchmarks.basic;

import brbo.benchmarks.Common;

abstract class Test01 extends Common {
  void f(int n) {
    if (n <= 0)
      return;
    int R = 0;
    boundAssertion (R <= 1);
    int i = 0;
    while (i < n) {
      // i++;
      R = R + 1;
    }
  }
}