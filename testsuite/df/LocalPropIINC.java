// Phase 2 test: IINC as intervening write should block propagation
// Expected: With *enable-local-propagation* = t, propagation should be blocked by iinc

public class LocalPropIINC {
    public static int testIINCBlocks(int x) {
        int z = x;  // local z = x (say x = 5)
        x++;        // iinc on x - intervening write!
        return z;   // Should return 5 (old x), NOT 6 (new x)
    }

    public static void main(String[] args) {
        int result = testIINCBlocks(5);
        if (result == 5) {
            System.out.println("PASS: LocalPropIINC");
        } else {
            System.out.println("FAIL: LocalPropIINC - expected 5, got " + result);
            System.exit(1);
        }
    }
}
