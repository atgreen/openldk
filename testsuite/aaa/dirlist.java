/* { dg-output "OK" } */

package aaa;

import java.io.File;
import java.nio.file.Files;

// File.list includes subdirectories, gives an empty array for an empty
// directory and null for a non-directory, as on a JVM.  It listed only
// files, and null for a directory holding nothing else, which broke
// Scala's compiler on a classpath directory of packages.
public class dirlist
{
    public static void main(String[] args) throws Exception
    {
        File root = Files.createTempDirectory("dirlist").toFile();
        File sub = new File(root, "pkg");
        File empty = new File(root, "empty");
        sub.mkdir();
        empty.mkdir();
        new File(sub, "A.class").createNewFile();
        String[] names = root.list();
        java.util.Arrays.sort(names);
        boolean ok = names.length == 2 && names[0].equals("empty") && names[1].equals("pkg")
            && sub.list().length == 1
            && empty.list().length == 0
            && new File(sub, "A.class").list() == null
            && new File(root, "missing").list() == null;
        new File(sub, "A.class").delete(); sub.delete(); empty.delete(); root.delete();
        System.out.println(ok ? "OK" : "FAIL: " + java.util.Arrays.toString(names));
    }
}
