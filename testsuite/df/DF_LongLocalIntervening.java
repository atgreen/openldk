// DataFlow test: long local intervening write must block propagation.
// Pattern:
//   long z = L;   // copy from long local
//   L = 7777777777L; // intervening write to the same local index
//   return z;     // must return original L

public class DF_LongLocalIntervening {
    static long g(long L) {
        long z = L;
        L = 7777777777L;  // reassign local
        return z;
    }

    public static void main(String[] args) {
        long a = g(1234567890123L);
        boolean ok = (a == 1234567890123L);
        if (!ok) {
            System.err.println("FAIL: DF_LongLocalIntervening expected 1234567890123 got " + a);
            System.exit(1);
        }
        System.out.println("PASS");
    }
}

