package brbo.archive.containers.digikam;

import brbo.benchmarks.Common;

abstract public class BoxIrefParse extends Common {
  void f(int range, int nRefs1, int nRefs2) {
    if (range <= 0 || nRefs1 <= 0 || nRefs2 <= 0)
      return;
    int i = 0;
    int R = 0;
    while (i < range) {
      if (ndBool())
        break;
      if (ndBool()) {
        for (int i1 = 0; i1 < nRefs1; i1++) {
          R = R + 1;
        }
      }
      i++;
    }
  }
}
