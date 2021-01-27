package brbo.benchmarks.string.apache.lang3;

import brbo.benchmarks.Common;

abstract public class AppendAllTo extends Common {
  void f(int sep, int types) {
    if (sep <= 0 || types <= 0)
      return;
    int builder = 0;
    int R = 0;
    boundAssertion(R <= types * sep + 1);
    builder++;
    R = R + 1;
    for (int i = 1; i < types; i++) {
      builder += sep;
      R = R + sep;
      builder++;
      R = R + 1;
    }
  }
}
