// Inter-block test: single reaching definition should be safe (no writes on any path).
// This should remain correct regardless of propagation (sanity guard).

public class DF_InterBlock_UniqueDef_OK {
    static void dummy() { /* force a call/branch split */ }

    static int f(int x) {
        int y = 10;        // single definition
        if (x >= 0) {      // forces a basic block split
            dummy();       // ensure non-empty block
        }
        return y;          // use after split
    }

    public static void main(String[] args) {
        int a = f(5);
        int b = f(-1);
        if (a != 10 || b != 10) {
            System.err.println("FAIL: DF_InterBlock_UniqueDef_OK expected 10,10 got " + a + "," + b);
            System.exit(1);
        }
        System.out.println("PASS");
    }
}

