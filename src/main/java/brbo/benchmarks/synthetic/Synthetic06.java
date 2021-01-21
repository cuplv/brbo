package brbo.benchmarks.synthetic;

import brbo.benchmarks.Common;

public abstract class Synthetic06 extends Common {
  void f(int n, int m, int l) {
    int R = 0;
    int i = 0;
    assume(n >= 0 && m >= 0 && l >= 0);
    assert (R <= l * m * n);
    while (i < n) {
      int j = 0;
      while (j < m) {
        int k = 0;
        while (k < l) {
          if (ndBool())  {
            R = R + 1;
          }
          else {
            R = R + (-1);
          }
          k++;
        }
        j++;
      }
      i++;
    }
  }
}