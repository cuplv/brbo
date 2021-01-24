package brbo.benchmarks.basic;

import brbo.benchmarks.Common;

abstract class Test01 extends Common {
  void f(int n) {
    int R = 0;
    boundAssertion (n < 0 || R <= n);
    int i = 0;
    while (i < n) {
      i++;
      R = R + 1;
    }
  }
}