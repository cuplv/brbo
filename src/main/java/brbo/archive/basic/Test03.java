package brbo.archive.basic;

class Test03 {
  void f(int n, int m) {
    int r = 0;
    int i = 0;
    while (i < n) {
      int j;
      j = 0;
      while (j < m) {
        j++;
        r++;
      }
      i++;
    }
  }
}