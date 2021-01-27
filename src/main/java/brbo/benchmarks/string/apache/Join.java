package brbo.benchmarks.string.apache;

import brbo.benchmarks.Common;

abstract public class Join extends Common {
  void f(int array, int startIndex, int endIndex) {
    if (array <= 0 || startIndex <= 0 || endIndex <= 0)
      return;
    int noOfItems = endIndex - startIndex;
    if (noOfItems <= 0)
      return;
    int R = 0;
    boundAssertion(R <= 2 * (endIndex - startIndex));
    int buf = 0;
    buf++;
    R = R + 1;
    for (int i = startIndex + 1; i < endIndex; i++) {
      buf++;
      R = R + 1;
      buf++;
      R = R + 1;
    }
  }
}
