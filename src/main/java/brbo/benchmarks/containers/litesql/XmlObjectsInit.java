package brbo.benchmarks.containers.litesql;

import brbo.benchmarks.Common;

abstract public class XmlObjectsInit extends Common {
  void f(int objects, int relations) {
    if (objects <= 0 || relations <= 0)
      return;
    int R = 0;
    mostPreciseBound(R <= objects + 2 * relations);
    for (int i = 0; i < objects; i++) {
      if (ndBool()) {
        R = R + 1;
      }
    }
    int iterator = relations;
    while (iterator > 0) {
      int rel = ndInt2(1, iterator);
      iterator -= rel;
      for (int i2 = 0; i2 < rel; i2++) {
        R = R + 1;
        if (ndBool()) {
          R = R + 1;
        }
      }
    }
  }
}
