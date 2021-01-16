package brbo.benchmarks.containers.litesql;

import brbo.benchmarks.Common;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public abstract class LitesqlGenCpp1 extends Common {
  void writeObjRelationHandles(int handles) {
    int R = 0;
    assert (handles < 0 || R <= (1));

    int iterator = handles;
    while (iterator > 0) {
      int handle = ndInt(1, iterator);
      iterator -= handle;

      int params = 0;
      params++;
      R = R + 1;

      int destObjects = ndInt(1, handle);
      int iterator1 = destObjects;
      while (iterator1 > 0) {
        iterator1--;
        params++;
        R = R + 1;
      }

      int rel = handle - destObjects;
      int fields = rel;
      int iterator2 = fields;
      while (iterator2 > 0) {
        iterator2--;
        params++;
        R = R + 1;
      }
      R = R + (-params);
      params = 0;

      params++;
      R = R + 1;
      params++;
      R = R + 1;

      if (ndBool()) {
        R = R + (-params);
        params = 0;

        params++;
        R = R + 1;
        params++;
        R = R + 1;
        params++;
        R = R + 1;
      } else {
        if (ndBool()) {
          int iterator3 = destObjects;
          while (iterator3 > 0) {
            iterator3--;
            R = R + (-params);
            params = 0;

            params++;
            R = R + 1;
            params++;
            R = R + 1;
            params++;
            R = R + 1;
          }
        } else {
          int iterator4 = destObjects;
          while (iterator4 > 0) {
            iterator4--;
            R = R + (-params);
            params = 0;

            params++;
            R = R + 1;
            params++;
            R = R + 1;
            params++;
            R = R + 1;
          }
        }
      }
    }
    // ?; ?; ?
  }
}