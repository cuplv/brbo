package brbo.benchmarks.containers.litesql;

import brbo.benchmarks.Common;

public abstract class Database extends Common {
  void upgradeTable(int oldFields, int newFields) {
    int r = 0;

    int toAdd = newFields;
    int i = 0;
    while (i < oldFields) {
      if (ndBool()) {
        toAdd--;
        r = r + (-1);
      }
      i++;
    }

    int commonFields = 0;
    i = 0;
    while (i < oldFields) {
      if (ndBool()) {
        commonFields++;
        r = r + 1;
      }
      i++;
    }

    int cols = 0;
    int colNames = 0;
    i = 0;
    while (i < commonFields) {
      cols++;
      colNames++;
      r = r + 1;
      i++;
    }

    i = 0;
    while (i < toAdd) {
      cols++;
      colNames++;
      r = r + 1;
      i++;
    }
  }
}
