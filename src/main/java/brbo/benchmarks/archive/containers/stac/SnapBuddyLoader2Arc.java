package brbo.benchmarks.archive.containers.stac;

import brbo.benchmarks.Common;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

public abstract class SnapBuddyLoader2Arc extends Common {
  void getPhotos(Map<List<Integer>, List<Integer>> lines) {
    int r = 0;

    List<List<Integer>> photoIdList = new ArrayList<>();
    List<List<Integer>> friendIdList = new ArrayList<>();
    Iterator<Map.Entry<List<Integer>, List<Integer>>> iterator = lines.entrySet().iterator();
    while (iterator.hasNext()) {
      Map.Entry<List<Integer>, List<Integer>> entry = iterator.next();

      List<Integer> photoIds = new ArrayList<>();
      List<Integer> photoIdentities = entry.getKey();
      Iterator<Integer> iterator1 = photoIdentities.iterator();
      while (iterator1.hasNext()) {
        Integer photoId = iterator1.next();
        if (ndBool()) {
          photoIds.add(photoId);
          r = r + 1;
        }
      }
      if (ndBool()) {
        photoIdList.add(photoIds);
      }

      List<Integer> friendIds = new ArrayList<>();
      List<Integer> friendIdentities = entry.getValue();
      Iterator<Integer> iterator2 = friendIdentities.iterator();
      while (iterator2.hasNext()) {
        Integer friendId = iterator2.next();
        if (ndBool()) {
          friendIds.add(friendId);
          r = r + 1;
        }
      }
      if (ndBool()) {
        friendIdList.add(friendIds);
      }
    }
  }
}