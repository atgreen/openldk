/* { dg-output "OK" } */

package aaa;

// Only real JDK packages are system packages: asking for one that doesn't
// exist gives null, not a made-up Package (Kawa resolved unknown names as
// java.lang.Package objects this way).
public class syspackage
{
    @SuppressWarnings("deprecation")
    public static void main(String[] args)
    {
        boolean missing = Package.getPackage("no.such.pkg") == null
            && Package.getPackage("no-such-proc") == null;
        boolean real = Package.getPackage("java.util") != null
            && String.class.getPackage().getName().equals("java.lang");
        System.out.println(missing && real ? "OK" : "FAIL: missing=" + missing + " real=" + real);
    }
}
