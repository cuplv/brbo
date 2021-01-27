package brbo.benchmarks.string.apache.stringutils;

import brbo.benchmarks.Common;

abstract public class SplitWorker2 extends Common {
  void f(int str, int separatorChars, int max, boolean preserveAllTokens) {
    if (str <= 0 || max <= 0 || separatorChars < 0)
      return;
    int R = 0;
    boundAssertion(R <= str);
    int list = 0;
    int sizePlus1 = 1;
    int i = 0, start = 0;
    boolean match = false;
    boolean lastMatch = false;
    int len = str;
    if (separatorChars == 0) {
      while (i < len) {
        if (ndBool()) {
          if (match || preserveAllTokens) {
            lastMatch = true;
            if (sizePlus1++ == max) {
              i = len;
              lastMatch = false;
            }
            list += i - start;
            R = R + (i - start);
            match = false;
          }
          start = ++i;
          continue;
        }
        lastMatch = false;
        match = true;
        i++;
      }
    } else if (separatorChars == 1) {
      while (i < len) {
        if (ndBool()) {
          if (match || preserveAllTokens) {
            lastMatch = true;
            if (sizePlus1++ == max) {
              i = len;
              lastMatch = false;
            }
            list += i - start;
            R = R + (i - start);
            match = false;
          }
          start = ++i;
          continue;
        }
        lastMatch = false;
        match = true;
        i++;
      }
    } else {
      while (i < len) {
        if (ndBool()) {
          if (match || preserveAllTokens) {
            lastMatch = true;
            if (sizePlus1++ == max) {
              i = len;
              lastMatch = false;
            }
            list += i - start;
            R = R + (i - start);
            match = false;
          }
          start = ++i;
          continue;
        }
        lastMatch = false;
        match = true;
        i++;
      }
    }
    if (match || preserveAllTokens && lastMatch) {
      list += i - start;
      R = R + (i - start);
    }
  }
}
