// Cross-block iinc test: post-branch increment must be visible at use.
// Propagation must not replace the read with a pre-increment value.

public class DF_IIncAcrossBlocks {
    static int m(int x, boolean inc) {
        if (inc) {
            x++;            // iinc on one path
        }
        return x;           // use after merge
    }

    public static void main(String[] args) {
        int a = m(7, false); // 7
        int b = m(7, true);  // 8
        if (a != 7 || b != 8) {
            System.err.println("FAIL: DF_IIncAcrossBlocks expected 7,8 got " + a + "," + b);
            System.exit(1);
        }
        System.out.println("PASS");
    }
}

