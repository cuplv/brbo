package brbo.benchmarks.string;

import brbo.benchmarks.Common;

abstract public class SubstringsBetween extends Common {
  void f(int str, int open, int close) {
    if (str <= 0 || open <= 0 || close <= 0)
      return;
    int R = 0;
    boundAssertion(R <= str);
    int list = 0;
    int pos = 0;
    while (pos < str - close) {
      int start = ndBool() ? -1 : ndInt2(pos, str - 1);
      if (start < 0) {
        break;
      }
      start += open;
      int end = ndBool() ? -1 : ndInt2(start, str - 1);
      if (end < 0) {
        break;
      }
      list += end - start;
      R = R + (end - start);
      pos = end + close;
    }
  }
}

