package brbo.benchmarks.synthetic;
import brbo.benchmarks.Common;
public abstract class Synthetic13 extends Common {
  void f(int n) {
    if (n <= 0)
      return;
    int R = 0;
    mostPreciseBound(R <= (n + 1));
    R = R + n;
    R = R + 1;
  }
}