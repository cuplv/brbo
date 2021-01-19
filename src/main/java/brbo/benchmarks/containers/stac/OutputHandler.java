package brbo.benchmarks.containers.stac;

import brbo.benchmarks.Common;

public abstract class OutputHandler extends Common {
  void addResultHelper(int map) {
    int R = 0;
    assert (map < 0 || R <= map);

    int iterator = map;
    while (iterator > 0) {
      int entry = ndInt2(1, iterator);
      iterator -= entry;
      if (ndBool()) {
        int list = ndInt();
        list++; // Initial: The loop
        R = R + 1;
      } else {
        int list2 = 0;
        list2++; // Initial: The loop
        R = R + 1;
      }
    }
    // Yes; Yes; Yes
  }
}
