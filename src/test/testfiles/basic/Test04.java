class Test04 {
    void f(int n, int m, int l) {
        int R = 0;
        int i = 0;
        while (i < n) {
            int j = 0;
            while (j < m) {
                j++;
                R++;
            }
        }

        for (int j = 0; j < l; j++) {
            R++;
        }

        int k = 0;
        do {
            i++;
            R++;
        } while (k < n);
    }
}