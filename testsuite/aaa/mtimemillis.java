/* { dg-output "OK" } */

package aaa;

import java.io.File;
import java.nio.file.Files;

// File.lastModified and Files.getLastModifiedTime keep the milliseconds
// of a modification time; OpenLDK rounded both to whole seconds.
public class mtimemillis
{
    public static void main(String[] args) throws Exception
    {
        File f = File.createTempFile("mtime", ".txt");
        boolean ok = f.setLastModified(1234567890123L)
            && f.lastModified() == 1234567890123L
            && Files.getLastModifiedTime(f.toPath()).toMillis() == 1234567890123L;
        long a = f.lastModified(), b = Files.getLastModifiedTime(f.toPath()).toMillis();
        f.delete();
        System.out.println(ok ? "OK" : "FAIL: " + a + " " + b);
    }
}
