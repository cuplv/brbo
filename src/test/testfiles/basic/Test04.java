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

        i = 0;
        while (i < l) {
            i++;
            R++;
        }
    }
}