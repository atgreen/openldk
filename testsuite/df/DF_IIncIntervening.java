// DataFlow test: iinc intervening write must block local propagation.
// Pattern:
//   int z = x;   // copy from local
//   x++;         // intervening write via IINC
//   return z;    // must return original x
// If propagation incorrectly replaces z with x, result will be incremented.

public class DF_IIncIntervening {
    static int f(int x) {
        int z = x;
        x++;             // javac should emit IINC here
        return z;
    }

    public static void main(String[] args) {
        int a = f(7);
        int b = f(-3);
        boolean ok = (a == 7) && (b == -3);
        if (!ok) {
            System.err.println("FAIL: DF_IIncIntervening expected 7,-3 got " + a + "," + b);
            System.exit(1);
        }
        System.out.println("PASS");
    }
}

