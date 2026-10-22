/* { dg-output "OK" } */

package aaa;

import java.io.*;
import java.util.*;
import java.util.zip.*;

// ZipFile.entries and size (Scala's compiler lists jars this way), with
// entry sizes.
public class zipentries
{
    public static void main(String[] args) throws Exception
    {
        File f = File.createTempFile("zipentries", ".zip");
        try (ZipOutputStream out = new ZipOutputStream(new FileOutputStream(f))) {
            for (String n : new String[] { "a.txt", "dir/b.txt" }) {
                out.putNextEntry(new ZipEntry(n));
                out.write((n + n).getBytes("UTF-8"));
                out.closeEntry();
            }
        }
        List<String> seen = new ArrayList<>();
        try (ZipFile z = new ZipFile(f)) {
            for (Enumeration<? extends ZipEntry> e = z.entries(); e.hasMoreElements(); ) {
                ZipEntry ze = e.nextElement();
                seen.add(ze.getName() + ":" + ze.getSize());
            }
            seen.add("size=" + z.size());
        }
        f.delete();
        Collections.sort(seen);
        String result = seen.toString();
        System.out.println("[a.txt:10, dir/b.txt:18, size=2]".equals(result) ? "OK" : "FAIL: " + result);
    }
}
