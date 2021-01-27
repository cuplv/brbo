package brbo.benchmarks.string;

import brbo.benchmarks.Common;

abstract public class Replace extends Common {
  void f(int text, int searchString, int replacement, int max) {
    if (text <= 0 || searchString <= 0 || max <= 0 || replacement <= 0)
      return;
    int start = 0;
    int end = ndBool() ? -1 : ndInt2(start, text - 1);
    if (end == -1)
      return;
    int R = 0;
    boundAssertion(R <= text + text * replacement);
    int replLength = searchString;
    int buf = 0;
    while (end != -1) {
      buf += end - start;
      R = R + (end - start);
      buf += replacement;
      R = R + replacement;
      start = end + replLength;
      max--;
      if (max == 0) {
        break;
      }
      end = ndBool() ? -1 : ndInt2(start, text - 1);
    }
    buf += text - start;
    R = R + (text - start);
  }
}
