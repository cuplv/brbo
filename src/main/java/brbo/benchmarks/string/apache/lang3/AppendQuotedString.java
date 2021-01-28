package brbo.benchmarks.string.apache.lang3;

import brbo.benchmarks.Common;

abstract public class AppendQuotedString extends Common {
  void f(int pattern, int pos) {
    if (pattern <= 0 || pos <= 0 || pattern < pos)
      return;
    int appendTo = 0;
    int R = 0;
    boundAssertion(R <= pattern);
    int start = pos;
    int lastHold = start;
    for (int i = pos; i < pattern; i++) {
      if (ndBool()) {
        pos++;
        appendTo += pos - lastHold;
        R = R + (pos - lastHold);
        break;
      }
      pos++;
    }
  }
}
