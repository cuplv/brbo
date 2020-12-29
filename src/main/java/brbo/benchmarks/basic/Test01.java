package brbo.benchmarks.basic;

class Test01 {
  void f(int n) {
    int r = 0;
    int i = 0;
    while (i < n) {
      i++;
      r = r + 1;
      r = r + 2;
    }
  }
}