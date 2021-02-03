package brbo.archive.unjustified.synthetic;

import brbo.benchmarks.Common;

public abstract class Synthetic06 extends Common {
  void f(int n, int m, int l) {
    if (n <= 0 || m <= 0 || l <= 0)
      return;
    int R = 0;
    int i = 0;
    mostPreciseBound(R <= l * m * n);
    lessPreciseBound(R <= 2 * l * m * n);
    while (i < n) {
      int j = 0;
      while (j < m) {
        int k = 0;
        while (k < l) {
          if (ndBool())  {
            R = R + 1;
          }
          else {
            R = R + 1;
          }
          k++;
        }
        j++;
      }
      i++;
    }
  }
}