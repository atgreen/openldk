/* { dg-output "OK" } */

package aaa;

import java.io.File;
import java.io.FileOutputStream;
import java.util.Arrays;

// JDK 25 calls File's natives getLength0, getLastModifiedTime0 and list0,
// which OpenLDK only knew by their JDK 8 names.  A missing file reports 0.
public class filestat
{
    public static void main(String[] args) throws Exception
    {
        File f = new File("filestat.tmp");
        FileOutputStream out = new FileOutputStream(f);
        out.write(new byte[] { 1, 2, 3, 4, 5 });
        out.close();
        boolean ok = f.length() == 5;
        ok &= f.lastModified() > 0;
        f.delete();

        File missing = new File("no-such-file.tmp");
        ok &= missing.length() == 0;
        ok &= missing.lastModified() == 0;

        String[] names = new File("aaa").list();
        ok &= names != null && Arrays.asList(names).contains("filestat.java");
        System.out.println(ok ? "OK" : "FAIL");
    }
}
