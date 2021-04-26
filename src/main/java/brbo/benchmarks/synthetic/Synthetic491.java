package brbo.benchmarks.synthetic;
import brbo.benchmarks.Common;
public abstract class Synthetic491 extends Common {
  void f(int n) {
    if (n <= 0) {
      return;
    }
    int R = 0;
    mostPreciseBound(R <= ((n + n * (1 + n)) + n * n * 1));
    lessPreciseBound(R <= ((n + n * (1 + n)) + n * n * 1) * 8);
    for (int it0 = n, entry0 = ndInt2(1, it0); it0 > 0; it0 -= entry0, entry0 = ndInt2(1, it0)) {
      R = R + entry0;
      for (int i0 = 0; i0 < entry0; i0++) {
        R = R + 1;
        R = R + n;
      }
    }
    for (int i1 = 0; i1 < n; i1++) {
      for (int i2 = 0; i2 < n; i2++) {
        R = R + 1;
      }
    }
  }
}