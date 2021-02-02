package brbo.archive.unjustified.containers.stac;

import brbo.benchmarks.Common;

public abstract class SnapBuddyLoader2 extends Common {
  void getPhotos(int lines) {
    if (lines <= 0)
      return;
    int R = 0;
    mostPreciseBound(R <= lines);
    lessPreciseBound(R <= 2 * lines);

    int photoIdList = 0;
    int friendIdList = 0;

    int iterator = lines;
    while (iterator > 0) {
      int entry = ndInt2(1, iterator);
      iterator -= entry;

      int photoIds = 0;
      int photoIdentities = ndInt2(1, entry);
      int iterator1 = photoIdentities;
      while (iterator1 > 0) {
        iterator1--;
        if (ndBool()) {
          photoIds++;
          R = R + 1;
        }
      }
      if (ndBool()) {
        photoIdList++;
      }

      int friendIds = 0;
      int friendIdentities = entry - photoIdentities;
      int iterator2 = friendIdentities;
      while (iterator2 > 0) {
        iterator2--;
        if (ndBool()) {
          friendIds++;
          R = R + 1;
        }
      }
      if (ndBool()) {
        friendIdList++;
      }
    }
    // No (Imprecise due to interference); No (Imprecise due to interference); No (Cannot verify complex invariants inside loops)
  }
}