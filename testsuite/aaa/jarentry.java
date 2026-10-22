/* { dg-output "OK" } */

package aaa;

import java.io.File;
import java.io.FileOutputStream;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.jar.JarOutputStream;

// JarFile.getJarEntry returns a JarEntry: it casts getEntry's result,
// which was a plain ZipEntry (Groovy's compiler failed on its jars).
public class jarentry
{
    public static void main(String[] args) throws Exception
    {
        File jar = File.createTempFile("jarentry", ".jar");
        try (JarOutputStream out = new JarOutputStream(new FileOutputStream(jar))) {
            out.putNextEntry(new JarEntry("a/b.txt"));
            out.write("hi".getBytes());
            out.closeEntry();
        }
        try (JarFile file = new JarFile(jar)) {
            JarEntry entry = file.getJarEntry("a/b.txt");
            System.out.println(entry != null && entry.getName().equals("a/b.txt")
                               && file.getJarEntry("missing") == null ? "OK" : "FAIL");
        } finally {
            jar.delete();
        }
    }
}
