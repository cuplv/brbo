package brbo.benchmarks.containers.stac;

import brbo.benchmarks.Common;

public abstract class SnapBuddyLoader2 extends Common {
  void getPhotos(int lines) {
    int R = 0;
    assert (lines < 0 || R <= lines);

    int photoIdList = 0;
    int friendIdList = 0;

    int iterator = lines;
    while (iterator > 0) {
      int entry = ndInt(2, iterator);
      iterator -= entry;

      int photoIds = 0;
      int photoIdentities = ndInt(1, entry);
      int iterator1 = entry;
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
    // No (Imprecise due to interference); No (Same as ICRA); No (Cannot verify complex invariants inside loops)
  }
}