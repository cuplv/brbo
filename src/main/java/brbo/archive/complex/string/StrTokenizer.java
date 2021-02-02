package brbo.archive.complex.string;

import brbo.benchmarks.Common;

abstract public class StrTokenizer extends Common {
  void f(int srcChars, int start, int len,
         int tokenList, int quoteStart, int quoteLen) {
    if (srcChars <= 0 || start <= 0 || quoteLen < 0)
      return;
    int workArea = 0;
    int pos = start;
    boolean quoting = quoteLen > 0;
    int trimStart = 0;
    while (pos < len) {

    }
  }
}
