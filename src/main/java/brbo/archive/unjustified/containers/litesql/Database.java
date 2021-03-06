package brbo.archive.unjustified.containers.litesql;

import brbo.benchmarks.Common;

public abstract class Database extends Common {
  void upgradeTable(int oldFields, int newFields) {
    if (oldFields <= 0 || newFields <= 0)
      return;
    int R = 0;
    mostPreciseBound(R <= (oldFields + newFields));

    int toAdd = 0;
    toAdd += newFields;
    int iterator = oldFields;
    while (iterator > 0) {
      iterator--;
      if (ndBool()) {
        toAdd--;
      }
    }

    int commonFields = 0;
    int iterator1 = oldFields;
    while (iterator1 > 0) {
      iterator1--;
      if (ndBool()) {
        commonFields++;
      }
    }

    int iterator2 = toAdd;
    while (iterator2 > 0) {
      iterator2--;
    }

    int cols = 0;
    int colNames = 0;
    int iterator3 = commonFields;
    while (iterator3 > 0) {
      iterator3--;
      cols++;
      R = R + 1;
      colNames++;
    }

    int iterator4 = toAdd;
    while (iterator4 > 0) {
      iterator4--;
      cols++;
      R = R + 1;
      colNames++;
    }
    // Yes; Yes ; Yes
  }
}
