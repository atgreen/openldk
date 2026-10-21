/* { dg-output "OK" } */

package aaa;

// When nested trys in one method can both catch an exception, the inner one
// catches it, as the exception table's order says.  OpenLDK ordered the
// clauses of its one HANDLER-CASE by range end, so the outer try won.
public class catchorder {
    static void boom() { throw new IllegalStateException("boom"); }
    static StringBuilder log = new StringBuilder();

    // Both catch RuntimeException; the inner one must.
    static String sameType() {
        try {
            try { boom(); return "normal"; }
            catch (RuntimeException e) { log.append("inner "); }
            return "inner";
        } catch (RuntimeException e) {
            return "outer";
        }
    }

    // An inner finally runs before the outer catch.
    static String finallyFirst() {
        try {
            try { boom(); return "normal"; }
            finally { log.append("finally "); }
        } catch (RuntimeException e) {
            return "outer";
        }
    }

    public static void main(String[] a) {
        String r1, r2;
        try { r1 = sameType(); } catch (Throwable t) { r1 = "escaped: " + t; }
        try { r2 = finallyFirst(); } catch (Throwable t) { r2 = "escaped: " + t; }
        String all = r1 + " " + r2 + " " + log;
        System.out.println("inner outer inner finally ".equals(all) ? "OK" : "FAIL: " + all);
    }
}
