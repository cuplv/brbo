package brbo.benchmarks.containers.log4j2;

import brbo.benchmarks.Common;

abstract public class GetAllDeclaredFields extends Common {
  void f(int cls) {
    if (cls <= 0)
      return;
    int iterator = cls;
    int R = 0;
    boundAssertion(R <= cls);
    int fields = 0;
    while (iterator > 0) {
      int entry = ndInt2(1, iterator);
      iterator -= entry;
      fields += entry;
      R = R + entry;
    }
  }
}
