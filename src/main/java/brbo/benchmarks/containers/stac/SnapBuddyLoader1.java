package brbo.benchmarks.containers.stac;

import brbo.benchmarks.Common;

public abstract class SnapBuddyLoader1 extends Common {
  void getPhotos(int lines) {
    int R = 0;
    assume (lines > 0);
    boundAssertion (R <= lines);

    int photos = 0;
    int iterator = lines;
    while (iterator > 0) {
      int entry = ndInt2(1, iterator);
      iterator -= entry;
      int filters = 0;
      int filterIds = entry - 1;
      int iterator1 = filterIds;
      while (iterator1 > 0) {
        iterator1--;
        if (ndBool()) {
          filters++;
          R = R + 1; // Initial AST: The loop body of the inner loop
        }
      }
      if (ndBool()) {
        photos++;
      }
    }
    // Yes; Yes; Yes
  }
}