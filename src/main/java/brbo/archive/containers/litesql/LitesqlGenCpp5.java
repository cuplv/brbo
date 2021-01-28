package brbo.archive.containers.litesql;

import brbo.benchmarks.Common;

import java.util.ArrayList;
import java.util.List;

public abstract class LitesqlGenCpp5 extends Common {
  void writeObjConstructors(List<Integer> related, List<Integer> fields) {
    int r = 0;

    List<List<Integer>> cl = new ArrayList<>();
  }

  class Relation {
    List<Integer> related;
    List<Integer> fields;
  }
}