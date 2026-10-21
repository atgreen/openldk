/* { dg-output "OK" } */

package aaa;

// An exception thrown in an inner catch block is caught by an outer try in
// the same method.  OpenLDK merged the outer try's two ranges, the second
// starting at the inner handler, into one, and the handler went unprotected.
public class nestedcatch {
    static class Called extends RuntimeException { Called() { super("called"); } }
    static class Wrapped extends RuntimeException { Wrapped() { super("wrapped"); } }
    static void boom() { throw new Wrapped(); }
    static void escape() { throw new Called(); }
    // The inner catch block throws; the outer try, in the same method, must catch it.
    static Object f() {
        try {
            try { boom(); return "normal"; }
            catch (Throwable t) { escape(); return "unreached"; }
        } catch (Called c) {
            return "handled";
        }
    }
    public static void main(String[] a) {
        Object result;
        try { result = f(); } catch (Throwable t) { result = "escaped: " + t; }
        System.out.println("handled".equals(result) ? "OK" : "FAIL: " + result);
    }
}
