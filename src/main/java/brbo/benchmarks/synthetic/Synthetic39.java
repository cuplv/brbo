package brbo.benchmarks.synthetic;
import brbo.benchmarks.Common;
public abstract class Synthetic39 extends Common {
  void f(int n) {
    if (n <= 0)
      return;
    int R = 0;
    mostPreciseBound(R <= (n * n + 1));
    for (int it0 = n, entry0 = ndInt2(1, it0); it0 > 0; it0 -= entry0, entry0 = ndInt2(1, it0)) {
      R = R + n;
    }
    R = R + 1;
  }
}