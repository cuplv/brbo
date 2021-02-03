package brbo.benchmarks.synthetic;
import brbo.benchmarks.Common;
public abstract class Synthetic30 extends Common {
  void f(int n) {
    if (n <= 0)
      return;
    int R = 0;
    mostPreciseBound(R <= (n + n));
    R = R + n;
    R = R + n;
  }
}