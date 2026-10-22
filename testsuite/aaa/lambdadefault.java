/* { dg-output "OK" } */

package aaa;

// A lambda for an application interface gets its default methods (Groovy's
// CompilationUnit phase operations call a default doPhaseOperation).
// OpenLDK looked the interface's CLOS class up in its own package, not the
// application's, so the lambda implemented only Object.
public class lambdadefault
{
    interface Op {
        void call(StringBuilder out);
        default void run(StringBuilder out) { out.append('['); call(out); out.append(']'); }
    }

    public static void main(String[] args)
    {
        StringBuilder out = new StringBuilder();
        Op op = o -> o.append("x");
        op.run(out);
        Object asObject = op;
        boolean isOp = asObject instanceof Op;
        System.out.println("[x]".equals(out.toString()) && isOp ? "OK" : "FAIL: " + out + " " + isOp);
    }
}
