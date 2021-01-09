package brbo.benchmarks.containers.litesql;

import brbo.benchmarks.Common;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public abstract class LitesqlGenCpp3 extends Common {
  void writeStaticRelData(List<Integer> related, List<Integer> fields) {
    int r = 0;

    List<List<Integer>> cl = new ArrayList<>();
  }

  class Relation {
    List<Integer> related;
    List<Integer> fields;
  }
}