/* { dg-output "OK" } */

package aaa;

import java.io.InputStream;
import java.net.URL;

// A file: URL naming a device streams it.  OpenLDK read a file: URL's
// target into memory, and file:/dev/random -- which SecureRandom's seed
// generator reads -- never ends: the heap ran out.
public class devrandom
{
    public static void main(String[] args) throws Exception
    {
        byte[] bytes = new byte[20];
        int n;
        try (InputStream in = new URL("file:/dev/urandom").openConnection().getInputStream()) {
            n = in.read(bytes);
        }
        System.out.println(n > 0 ? "OK" : "FAIL: " + n);
    }
}
