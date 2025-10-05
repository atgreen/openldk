// Phase 2 test: Local variable propagation when safe (no intervening writes)
// Expected: With *enable-local-propagation* = t, local temp should propagate to both uses

public class LocalPropPositive {
    public static int testSimpleProp(int x) {
        int temp = x;
        return temp + temp;  // Should propagate x to both uses
    }

    public static void main(String[] args) {
        int result = testSimpleProp(5);
        if (result == 10) {
            System.out.println("PASS: LocalPropPositive");
        } else {
            System.out.println("FAIL: LocalPropPositive - expected 10, got " + result);
            System.exit(1);
        }
    }
}
