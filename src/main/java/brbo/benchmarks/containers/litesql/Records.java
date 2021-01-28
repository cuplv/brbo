package brbo.benchmarks.containers.litesql;

import brbo.benchmarks.Common;

abstract public class Records extends Common {
  void f(int recordNum, int fieldnum) {
    if (recordNum <= 0 || fieldnum <= 0)
      return;
    int R = 0;
    boundAssertion(R <= recordNum * fieldnum);
    for (int i = 0; i < recordNum; i++) {
      if (ndBool()) {
        break;
      }
      for (int i2 = 0; i2 < fieldnum; i2++) {
        R = R + 1;
      }
    }
  }
}
