// Loop test: local updated in loop; use after loop must reflect updates, not initial value.
// Propagation across blocks must not replace with the init literal.

public class DF_Loop_KillsPropagation {
    static int h(int n) {
        int y = 0;
        for (int i = 0; i < n; i++) {
            y = y + 1;
        }
        return y;
    }

    public static void main(String[] args) {
        int a = h(0);  // expect 0
        int b = h(5);  // expect 5
        if (a != 0 || b != 5) {
            System.err.println("FAIL: DF_Loop_KillsPropagation expected 0,5 got " + a + "," + b);
            System.exit(1);
        }
        System.out.println("PASS");
    }
}

