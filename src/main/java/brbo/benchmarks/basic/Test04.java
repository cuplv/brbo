package brbo.benchmarks.basic;

class Test04 {
  void f(int n, int m, int l) {
    int r = 0;
    int i = 0;
    while (i < n) {
      int j = 0;
      while (j < m) {
        j++;
        r++;
      }
      i++;
    }

    for (int j = 0; j < l; j++) {
      r++;
    }

    int k = 0;
    do {
      k++;
      r++;
    } while (k < n);
  }
}