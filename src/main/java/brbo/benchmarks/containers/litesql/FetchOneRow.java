package brbo.benchmarks.containers.litesql;

import brbo.benchmarks.Common;

abstract public class FetchOneRow extends Common {
  void f(int nFields) {
    if (nFields <= 0)
      return;
    int R = 0;
    mostPreciseBound(R <= nFields);
    for (int i2 = 1; i2 <= nFields; i2++) {
      if (ndBool()) {
        if (ndBool()) {
          if (ndBool()) {
            R = R + 1;
          }
          else {
            R = R + 1;
          }
        }
        else {
          R = R + 1;
        }
      }
      else if (ndBool()) {
        R = R + 1;
      }
      else {
        R = R + 1;
      }
    }
  }
}
