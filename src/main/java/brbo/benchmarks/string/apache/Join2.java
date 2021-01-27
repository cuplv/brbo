package brbo.benchmarks.string.apache;

import brbo.benchmarks.Common;

abstract public class Join2 extends Common {
  void f(int n) {
    if (n <= 0)
      return;
    int iterator = n;
    int R = 0;
    boundAssertion(R <= 2 * n);
    int buf = 0;
    iterator--;
    if (iterator <= 0)
      return;

    if (ndBool()) {
      buf++;
      R = R + 1;
    }
    while (iterator > 0) {
      buf++;
      R = R + 1;
      if (ndBool()) {
        buf++;
        R = R + 1;
      }
    }
  }
}
