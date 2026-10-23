/* { dg-output "OK" } */

package aaa;

import java.io.File;
import java.net.JarURLConnection;
import java.net.URL;
import java.nio.file.Files;
import java.util.Collections;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.jar.JarOutputStream;

// What Spring's classpath scanning does: find a package's directory as a
// resource, then walk a jar from a jar: URL's JarURLConnection.  The
// directory was not found, and the connection had no jar file URL or entry,
// and a JarFile it gave could not list its entries.
public class jarscan
{
    public static void main(String[] args) throws Exception
    {
        String result = "OK";
        URL dir = jarscan.class.getClassLoader().getResource("aaa/");
        if (dir == null || !dir.toString().startsWith("file:"))
            result = "FAIL: directory resource " + dir;

        File jar = File.createTempFile("jarscan", ".jar");
        try (JarOutputStream out = new JarOutputStream(Files.newOutputStream(jar.toPath()))) {
            out.putNextEntry(new JarEntry("a/"));
            out.closeEntry();
            out.putNextEntry(new JarEntry("a/b.txt"));
            out.write("hi".getBytes());
            out.closeEntry();
        }
        JarURLConnection con =
            (JarURLConnection) new URL("jar:file:" + jar.getPath() + "!/a/").openConnection();
        if (!("file:" + jar.getPath()).equals(String.valueOf(con.getJarFileURL())))
            result = "FAIL: jar file URL " + con.getJarFileURL();
        else if (!"a/".equals(con.getEntryName()) || con.getJarEntry() == null)
            result = "FAIL: entry " + con.getEntryName();
        else {
            try (JarFile file = con.getJarFile()) {
                int entries = Collections.list(file.entries()).size();
                String text = new String(file.getInputStream(file.getJarEntry("a/b.txt")).readAllBytes());
                if (entries != 2 || !"hi".equals(text))
                    result = "FAIL: " + entries + " entries, read " + text;
            }
        }
        jar.delete();
        System.out.println(result);
    }
}
