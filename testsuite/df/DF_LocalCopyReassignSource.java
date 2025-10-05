// DataFlow test: ensure locals are not incorrectly propagated through reassignments.
// Pattern:
//   int z = x;   // copy from local
//   x = 99;      // reassign source local
//   return z;    // must return original x
// If a naive propagation replaced z with x, the result would be 99 instead of 5.

public class DF_LocalCopyReassignSource {
    static int f(int x) {
        int z = x;
        x = 99; // intervening assignment to the source local
        return z;
    }

    public static void main(String[] args) {
        int got = f(5);
        if (got != 5) {
            System.err.println("FAIL: expected 5, got " + got);
            System.exit(1);
        }
        System.out.println("PASS");
    }
}

