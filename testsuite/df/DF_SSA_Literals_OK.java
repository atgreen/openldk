// DataFlow test: literals and SSA (stack) copies are fine to propagate.
// Behavior check: simple arithmetic with intermediate temps returns expected result.

public class DF_SSA_Literals_OK {
    static int h(int a, int b) {
        // Encourage stack temps: iload, iadd, istore to new local, etc.
        int one = 1;          // literal
        int two = 2;          // literal
        int s1 = a + one;     // SSA temp in IR then stored
        int s2 = b + two;     // SSA temp in IR then stored
        return s1 + s2;       // overall expected = (a+1) + (b+2)
    }

    public static void main(String[] args) {
        int r = h(3, 4);
        if (r != (3 + 1) + (4 + 2)) {
            System.err.println("FAIL: expected " + ((3+1)+(4+2)) + ", got " + r);
            System.exit(1);
        }
        System.out.println("PASS");
    }
}

