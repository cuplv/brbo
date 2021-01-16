package brbo.benchmarks.archive.containers.litesql;

import brbo.benchmarks.Common;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public abstract class DatabaseArc extends Common {
  void upgradeTable(List<Integer> oldFields, List<Integer> newFields) {
    int r = 0;

    List<Integer> toAdd = new ArrayList<>();
    toAdd.addAll(newFields);
    Iterator<Integer> iterator = oldFields.iterator();
    while (iterator.hasNext()) {
      Integer element = iterator.next();
      if (ndBool()) {
        toAdd.remove(element);
      }
    }

    List<Integer> commonFields = new ArrayList<>();
    Iterator<Integer> iterator1 = oldFields.iterator();
    while (iterator1.hasNext()) {
      Integer element1 = iterator1.next();
      if (ndBool()) {
        commonFields.add(element1);
      }
    }

    Iterator<Integer> iterator2 = toAdd.iterator();
    while (iterator2.hasNext()) {
      Integer element3 = iterator2.next();
    }

    List<Integer> cols = new ArrayList<>();
    List<Integer> colNames = new ArrayList<>();

    Iterator<Integer> iterator3 = commonFields.iterator();
    while (iterator3.hasNext()) {
      Integer element4 = iterator3.next();
      cols.add(element4);
      r = r + 1;
      colNames.add(element4);
    }

    Iterator<Integer> iterator4 = toAdd.iterator();
    while (iterator4.hasNext()) {
      Integer element5 = iterator4.next();
      cols.add(element5);
      r = r + 1;
      colNames.add(element5);
    }
  }
}
