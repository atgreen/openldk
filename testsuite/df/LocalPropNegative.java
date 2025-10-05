// Phase 2 test: Explicit assignment should block propagation
// Expected: With *enable-local-propagation* = t, propagation should be blocked by reassignment

public class LocalPropNegative {
    public static int testAssignmentBlocks(int x) {
        int temp = x;  // temp = x (say x = 5)
        x = 99;        // Reassignment to x - intervening write!
        return temp + x;  // temp should NOT propagate (would give wrong result)
    }

    public static void main(String[] args) {
        int result = testAssignmentBlocks(5);
        if (result == 104) {  // 5 + 99 = 104
            System.out.println("PASS: LocalPropNegative");
        } else {
            System.out.println("FAIL: LocalPropNegative - expected 104, got " + result);
            System.exit(1);
        }
    }
}
