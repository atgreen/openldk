/* { dg-output "OK" } */

package aaa;

// long | long is a signed long: FLAG | n, with FLAG's sign bit set, is
// negative.  It came out as the unsigned value, so Kawa's
// SimpleVector.isVerySimple (info < 0) took it for a subrange and indexed
// past its argument list.
public class longor
{
    static final long FLAG = Long.MIN_VALUE;
    static long info;

    static long make(int size) { return FLAG | size; }

    public static void main(String[] args)
    {
        info = make(3);
        long a = 0x8000000000000001L | 2L;
        boolean ok = info < 0 && info == -9223372036854775805L
            && a == -9223372036854775805L
            && (-1L | 0L) == -1L
            && (5L | 2L) == 7L;
        System.out.println(ok ? "OK" : "FAIL: " + info + " " + a);
    }
}
