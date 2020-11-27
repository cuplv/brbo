class Test04 {
    void f(int n, int m, int l) {
        int D1 = 0;
        int D2 = 0;
        int i = 0;
        while (i < n) {
            int j = 0;
            while (j < m) {
                j++;
                D1++;
            }
        }

        for (int j = 0; j < l; j++) {
            D1++;
        }

        int k = 0;
        do {
            i++;
            D2++;
        } while (k < n);
    }
}