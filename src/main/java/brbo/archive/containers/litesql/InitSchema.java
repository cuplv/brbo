package brbo.archive.containers.litesql;

import brbo.benchmarks.Common;

abstract public class InitSchema extends Common {
  void f(int objects, int relations) {
    if (objects <= 0 || relations <= 0)
      return;
    int R = 0;
    int iterator = objects;
    while (iterator > 0) {
      // objects
      R = R + 1;
      // objects
      if (ndBool()) {
        R = R + 1;
      }
      else {
        R = R + 1;
      }
      int entry = ndInt2(1, iterator);
      iterator -= entry;
      int fields = ndInt2(1, entry);
      int indices = ndInt2(1, entry - fields);
      // 3 * objects
      for (int i2 = 0; i2 < fields; i2++) {
        R = R + 1;
        if (ndBool()) {
          R = R + 1;
          R = R + 1;
        }
      }
      int iterator2 = indices;
      while (iterator2 > 0) {
        int entry2 = ndInt2(1, iterator2);
        iterator2 -= entry2;
        int idx = ndInt2(1, entry2);
        for (int i3 = 0; i3 < entry2; i3++) {
          R = R + 1; //
          R = R + 1; //
        }
        R = R + 1; // iterator
      }
    }
  }
}
