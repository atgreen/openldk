// DataFlow test: local defined from expression, then possibly redefined on a branch.
// Ensures that propagation respects possible intervening assignments.

public class DF_LocalCopyIfBranch {
    static int g(int x) {
        int y = x + 1;     // initial value
        if (x > 0) {
            y = x * 2;     // intervening assignment in same block region (javac keeps simple CFG)
        }
        return y;
    }

    public static void main(String[] args) {
        int a = g(0);  // expect 1
        int b = g(3);  // expect 6
        if (a != 1 || b != 6) {
            System.err.println("FAIL: expected a=1, b=6 but got a=" + a + ", b=" + b);
            System.exit(1);
        }
        System.out.println("PASS");
    }
}

