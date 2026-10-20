/* { dg-output "OK" } */

package aaa;

// Reading or writing a static field by reflection initializes its class
// first, so the value is the one <clinit> set, not the field's default.
public class reflstatic
{
    static class Reader
    {
        static String MSG = "set by <clinit>";
        static int N = 42;
    }

    static class Writer
    {
        static int N = 1;
        static int clinitRuns = 0;
        static { clinitRuns++; }
    }

    public static void main(String[] args) throws Exception
    {
        Object msg = Reader.class.getDeclaredField("MSG").get(null);
        Object n = Reader.class.getDeclaredField("N").get(null);
        // Field.set initializes the class too, so <clinit> can't overwrite
        // the value afterwards.
        Writer.class.getDeclaredField("N").set(null, 7);
        boolean ok = "set by <clinit>".equals(msg) && Integer.valueOf(42).equals(n)
            && Writer.N == 7 && Writer.clinitRuns == 1;
        System.out.println(ok ? "OK" : "FAIL: " + msg + " " + n + " " + Writer.N + " " + Writer.clinitRuns);
    }
}
