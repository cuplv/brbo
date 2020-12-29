package brbo.benchmarks.containers.stac;

import brbo.benchmarks.Common;

import java.util.*;

public abstract class OutputHandler extends Common {
  void addResultHelper(Map<Integer, Integer> map) {
    int r = 0;

    Map<Integer, List<Integer>> results = new HashMap<>();
    Iterator<Map.Entry<Integer, Integer>> iterator = map.entrySet().iterator();
    while (iterator.hasNext()) {
      Map.Entry<Integer, Integer> entry = iterator.next();
      Integer fileName = entry.getKey();
      Integer result = entry.getValue();
      if (results.containsKey(fileName)) {
        List<Integer> list = results.get(fileName);
        list.add(result);
        r = r + 1;
      } else {
        List<Integer> list = new ArrayList<>();
        list.add(result);
        results.put(fileName, list);
      }
    }
  }
}
