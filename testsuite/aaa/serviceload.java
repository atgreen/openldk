/* { dg-output "OK" } */

package aaa;

import java.util.ServiceLoader;

// Application code can use ServiceLoader (kotlin-reflect does at startup).
// OpenLDK reported the VM as still booting, when only java.base may.
public class serviceload
{
    public interface Plugin { String name(); }

    public static void main(String[] args)
    {
        int count = 0;
        for (Plugin p : ServiceLoader.load(Plugin.class)) count++;
        System.out.println(count == 0 ? "OK" : "FAIL: " + count);
    }
}
