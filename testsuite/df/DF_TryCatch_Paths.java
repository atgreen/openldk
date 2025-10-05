// Try/catch test: handler path reassigns local; value after try/catch depends on path.
// Propagation must not incorrectly fold to the pre-try value across the handler edge.

public class DF_TryCatch_Paths {
    static int k(boolean throwIt) {
        int y = 1;
        try {
            if (throwIt) {
                throw new RuntimeException();
            }
        } catch (RuntimeException e) {
            y = 2;
        }
        return y;
    }

    public static void main(String[] args) {
        int a = k(false); // 1
        int b = k(true);  // 2
        if (a != 1 || b != 2) {
            System.err.println("FAIL: DF_TryCatch_Paths expected 1,2 got " + a + "," + b);
            System.exit(1);
        }
        System.out.println("PASS");
    }
}

