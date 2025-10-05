// Phase 2 test: Long/double local variable propagation (2-slot locals)
// Expected: With *enable-local-propagation* = t, long locals should propagate when safe

public class LocalPropLong {
    public static long testLongProp(long x) {
        long temp = x;
        return temp + temp;  // Should propagate x to both uses
    }

    public static long testLongBlocked(long x) {
        long temp = x;  // temp = x
        x = 999L;       // Reassignment to x - blocks propagation
        return temp + x;
    }

    public static void main(String[] args) {
        long result1 = testLongProp(5000000000L);
        if (result1 == 10000000000L) {
            System.out.println("PASS: LocalPropLong.testLongProp");
        } else {
            System.out.println("FAIL: LocalPropLong.testLongProp - expected 10000000000, got " + result1);
            System.exit(1);
        }

        long result2 = testLongBlocked(5000000000L);
        if (result2 == 5000000999L) {  // 5000000000 + 999
            System.out.println("PASS: LocalPropLong.testLongBlocked");
        } else {
            System.out.println("FAIL: LocalPropLong.testLongBlocked - expected 5000000999, got " + result2);
            System.exit(1);
        }

        System.out.println("PASS: LocalPropLong");
    }
}
