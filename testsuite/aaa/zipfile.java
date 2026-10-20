/* { dg-output "OK" } */

package aaa;

import java.io.InputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

// java.util.zip.ZipFile is implemented in Lisp, and once kept its state in
// JDK 8's name and jzfile fields, which JDK 25's ZipFile no longer has, so
// opening any jar directly failed.
public class zipfile
{
    public static void main(String[] args) throws Exception
    {
        ZipFile zf = new ZipFile("test.jar");
        boolean ok = zf.getName().endsWith("test.jar");
        ok &= zf.getEntry("no/such/entry") == null;
        ZipEntry e = zf.getEntry("META-INF/MANIFEST.MF");
        ok &= e != null;
        StringBuilder sb = new StringBuilder();
        InputStream in = zf.getInputStream(e);
        for (int c; (c = in.read()) != -1; ) sb.append((char) c);
        ok &= sb.toString().startsWith("Manifest-Version");
        zf.close();
        System.out.println(ok ? "OK" : "FAIL");
    }
}
