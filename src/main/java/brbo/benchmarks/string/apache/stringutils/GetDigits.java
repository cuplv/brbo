package brbo.benchmarks.string.apache.stringutils;

import brbo.benchmarks.Common;

abstract public class GetDigits extends Common {
  void f(int str) {
    if (str <= 0)
      return;
    int R = 0;
    boundAssertion(R <= str);
    int sz = str;
    int strDigits = 0;
    for (int i = 0; i < sz; i++) {
      if (ndBool()) {
        strDigits++;
        R = R + 1;
      }
    }
  }
}
