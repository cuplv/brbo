package brbo.benchmarks.string.apache.lang3;

import brbo.benchmarks.Common;

abstract public class DbgPrint extends Common {
  void f(int src) {
    if (src <= 0)
      return;
    int sb = 0;
    int R = 0;
    boundAssertion(R <= sb);
    for (int i = 0; i < src; i++) {
      if (ndBool()) {
        sb++;
        R = R + 1;
      }
      else {
        sb++;
        R = R + 1;
      }
    }
  }
}