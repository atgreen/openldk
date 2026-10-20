/* { dg-output "OK" } */

package aaa;

// Calling a native method with no implementation throws
// UnsatisfiedLinkError, which Java code can catch, as on a JVM.
public class unsatisfiedlink
{
    static native int missingStatic(int x);
    native String missingInstance();

    public static void main(String[] args)
    {
        boolean staticCaught = false, instanceCaught = false;
        try {
            missingStatic(1);
        } catch (UnsatisfiedLinkError e) {
            staticCaught = e.getMessage() != null && e.getMessage().contains("missingStatic");
        }
        try {
            new unsatisfiedlink().missingInstance();
        } catch (UnsatisfiedLinkError e) {
            instanceCaught = true;
        }
        System.out.println(staticCaught && instanceCaught ? "OK" : "FAIL: " + staticCaught + " " + instanceCaught);
    }
}
