package brbo.benchmarks.containers.litesql;

import brbo.benchmarks.Common;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public abstract class LitesqlGenCpp1 extends Common {
  void writeObjRelationHandles(List<Handle> handles) {
    int r = 0;

    Iterator<Handle> iterator = handles.iterator();
    while (iterator.hasNext()) {
      Handle handle = iterator.next();
      List<Integer> params = new ArrayList<>();
      params.add(1);
      r = r + 1;

      List<Integer> destObjects = handle.destObjects;

      Iterator<Integer> iterator1 = destObjects.iterator();
      while (iterator1.hasNext()) {
        Integer o = iterator1.next();
        params.add(o);
        r = r + 1;
      }

      Relation rel = handle.relation;
      List<Integer> fields = rel.fields;
      Iterator<Integer> iterator2 = fields.iterator();
      while (iterator2.hasNext()) {
        Integer o2 = iterator2.next();
        params.add(o2);
        r = r + 1;
      }
      r = r + (-params.size());
      params.clear();

      params.add(1);
      r = r + 1;
      params.add(1);
      r = r + 1;

      if (ndBool()) {
        r = r + (-params.size());
        params.clear();

        params.add(1);
        r = r + 1;
        params.add(1);
        r = r + 1;
        params.add(1);
        r = r + 1;
      } else {
        if (ndBool()) {
          Iterator<Integer> iterator3 = destObjects.iterator();
          while (iterator3.hasNext()) {
            Integer o2 = iterator3.next();
            r = r + (-params.size());
            params.clear();

            params.add(1);
            r = r + 1;
            params.add(1);
            r = r + 1;
            params.add(1);
            r = r + 1;
          }
        } else {
          Iterator<Integer> iterator4 = destObjects.iterator();
          while (iterator4.hasNext()) {
            Integer o3 = iterator4.next();
            r = r + (-params.size());
            params.clear();

            params.add(1);
            r = r + 1;
            params.add(1);
            r = r + 1;
            params.add(1);
            r = r + 1;
          }
        }
      }
    }
  }

  class Handle {
    Relation relation;
    List<Integer> destObjects;
  }

  class Relation {
    List<Integer> fields;
  }
}