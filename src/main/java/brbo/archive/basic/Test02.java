package brbo.archive.basic;

class Test02 {
  void f(int n, int m) {
    int r = 0;
    int i = 0;
    while (i < n) {
      i++;
      r = r + 1;
    }

    i = 0;
    while (i < m) {
      i++;
      r = r + 1;
    }
  }
}