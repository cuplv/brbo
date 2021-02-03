package brbo.benchmarks.synthetic;
import brbo.benchmarks.Common;
public abstract class Synthetic35 extends Common {
  void f(int n) {
    if (n <= 0)
      return;
    int R = 0;
    mostPreciseBound(R <= n);
    R = R + n;
    for (int i0 = 0; i0 < n; i0++) {

    }
  }
}