package brbo.archive.complex.containers.stac;

import brbo.benchmarks.Common;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

public abstract class SnapBuddyLoader1Arc extends Common {
  void getPhotos(Map<Integer, List<Integer>> lines) {
    int r = 0;

    List<List<Integer>> photos = new ArrayList<>();
    Iterator<Map.Entry<Integer, List<Integer>>> iterator = lines.entrySet().iterator();
    while (iterator.hasNext()) {
      Map.Entry<Integer, List<Integer>> entry = iterator.next();
      List<Integer> filters = new ArrayList<>();
      List<Integer> filterIds = entry.getValue();
      Iterator<Integer> iterator1 = filterIds.iterator();
      while (iterator1.hasNext()) {
        Integer filter = iterator1.next();
        if (ndBool()) {
          filters.add(filter);
          r = r + 1;
        }
      }
      if (ndBool()) {
        photos.add(filters);
      }
    }
  }
}