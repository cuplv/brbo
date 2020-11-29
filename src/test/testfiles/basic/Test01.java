class Test01 {
    void f(int n) {
        int R = 0;
        int i = 0;
        while (i < n) {
            i++;
            R++;
            R = R + 2;
        }
    }
}