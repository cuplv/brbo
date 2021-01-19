package brbo.archive.containers.litesql;

import brbo.benchmarks.Common;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public abstract class LitesqlGenCpp2Arc extends Common {
  void writeRelMethods(List<Integer> related, List<Integer> fields) {
    int r = 0;

    List<List<Integer>> cl = new ArrayList<>();

    List<Integer> link = new ArrayList<>();

    link.add(1);
    r = r + 1;
    link.add(1);
    r = r + 1;

    Iterator<Integer> iterator = related.iterator();
    while (iterator.hasNext()) {
      Integer o = iterator.next();
      link.add(1);
      r = r + 1;
      link.add(1);
      r = r + 1;
    }

    Iterator<Integer> iterator1 = fields.iterator();
    while (iterator1.hasNext()) {
      Integer o1 = iterator1.next();
      link.add(1);
      r = r + 1;
      if (ndBool()) {
        link.add(1);
        r = r + 1;
      } else {
        link.add(1);
        r = r + 1;
      }
    }

    if (ndBool()) {
      link.add(1);
      r = r + 1;
      link.add(1);
      r = r + 1;

      Iterator<Integer> iterator2 = related.iterator();
      while (iterator2.hasNext()) {
        Integer o2 = iterator2.next();
        link.add(1);
        r = r + 1;
        link.add(1);
        r = r + 1;
      }

      Iterator<Integer> iterator3 = fields.iterator();
      while (iterator3.hasNext()) {
        Integer o3 = iterator3.next();
        link.add(1);
        r = r + 1;
        if (ndBool()) {
          link.add(1);
          r = r + 1;
        } else {
          link.add(1);
          r = r + 1;
        }
      }
      link.add(1);
      r = r + 1;
    }

    List<Integer> unlink = new ArrayList<>();
    Iterator<Integer> iterator4 = related.iterator();
    while (iterator4.hasNext()) {
      Integer o4 = iterator4.next();
      unlink.add(1);
      r = r + 1;
    }

    Iterator<Integer> iterator5 = fields.iterator();
    while (iterator5.hasNext()) {
      Integer o5 = iterator5.next();
      unlink.add(1);
      r = r + 1;
    }

    unlink.add(1);
    r = r + 1;
    if (ndBool()) {
      r = r + (-unlink.size());
      unlink.clear();

      Iterator<Integer> iterator6 = related.iterator();
      while (iterator6.hasNext()) {
        Integer o6 = iterator6.next();
        unlink.add(1);
        r = r + 1;
      }

      Iterator<Integer> iterator7 = fields.iterator();
      while (iterator7.hasNext()) {
        Integer o7 = iterator7.next();
        unlink.add(1);
        r = r + 1;
      }
      unlink.add(1);
      r = r + 1;
    }

    List<Integer> del = new ArrayList<>();
    del.add(1);
    r = r + 1;
    del.add(1);
    r = r + 1;
    del.add(1);
    r = r + 1;

    List<Integer> getRows = new ArrayList<>();
    getRows.add(1);
    r = r + 1;
    getRows.add(1);
    r = r + 1;
    getRows.add(1);
    r = r + 1;

    Iterator<Integer> iterator8 = related.iterator();
    while (iterator8.hasNext()) {
      Integer o8 = iterator8.next();
      getRows.add(1);
      r = r + 1;
    }

    Iterator<Integer> iterator9 = fields.iterator();
    while (iterator9.hasNext()) {
      Integer o9 = iterator9.next();
      getRows.add(1);
      r = r + 1;
    }
    getRows.add(1);
    r = r + 1;
    getRows.add(1);
    r = r + 1;
    getRows.add(1);
    r = r + 1;

    Iterator<Integer> iterator10 = related.iterator();
    while (iterator10.hasNext()) {
      Integer o10 = iterator10.next();
      link.add(1);
      r = r + 1;
      unlink.add(1);
      r = r + 1;
    }

    Iterator<Integer> iterator11 = fields.iterator();
    while (iterator11.hasNext()) {
      Integer o11 = iterator11.next();
      link.add(1);
      r = r + 1;
      unlink.add(1);
      r = r + 1;
    }

    cl.add(link);
    r = r + 1;
    cl.add(unlink);
    r = r + 1;
    cl.add(del);
    r = r + 1;
    cl.add(getRows);
    r = r + 1;
    if (ndBool()) {
      List<Integer> getTpl = new ArrayList<>();
      getTpl.add(1);
      r = r + 1;
      getTpl.add(1);
      r = r + 1;
      getTpl.add(1);
      r = r + 1;
      cl.add(getTpl);
      r = r + 1;

      Iterator<Integer> iterator12 = related.iterator();
      while (iterator12.hasNext()) {
        Integer o12 = iterator12.next();
        List<Integer> get = new ArrayList<>();
        get.add(1);
        r = r + 1;
        get.add(1);
        r = r + 1;
        get.add(1);
        r = r + 1;
        get.add(1);
        r = r + 1;
        get.add(1);
        r = r + 1;
        get.add(1);
        r = r + 1;
        get.add(1);
        r = r + 1;
        get.add(1);
        r = r + 1;

        cl.add(get);
        r = r + 1;
      }
    } else {
      List<Integer> counter = new ArrayList<>();
      Iterator<Integer> iterator13 = related.iterator();
      while (iterator13.hasNext()) {
        Integer o13 = iterator13.next();

        if (ndBool()) {
          counter.add(1);
          r = r + 1;
        }

        List<Integer> get = new ArrayList<>();
        get.add(1);
        r = r + 1;
        get.add(1);
        r = r + 1;
        get.add(1);
        r = r + 1;
        get.add(1);
        r = r + 1;
        get.add(1);
        r = r + 1;
        get.add(1);
        r = r + 1;
        get.add(1);
        r = r + 1;
        get.add(1);
        r = r + 1;

        cl.add(get);
        r = r + 1;
      }
    }
  }
}