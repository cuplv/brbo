class Test04 {
    void f(int n, int m, int l) {
        int D1 = 0;
        int i = 0;
        while (i < n) {
            int j = 0;
            while (j < m) {
                j++;
                D1++;
            }
        }

        i = 0;
        while (i < l) {
            i++;
            D1++;
        }
    }
}