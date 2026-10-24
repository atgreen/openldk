/* { dg-output "OK" } */

package aaa;

import java.io.*;
import java.net.*;
import java.nio.file.*;
import java.util.*;
import java.util.jar.*;
import java.util.zip.*;

// A jar's directory entry ends in /, and lookups find it without one too:
// the webjars locator checks for META-INF/resources/webjars/NAME/VERSION
// before Spring writes a versioned webjar link.
public class jardirs
{
    public static void main(String[] args) throws Exception
    {
        Path jar = Files.createTempFile("jardirs", ".jar");
        try (JarOutputStream out = new JarOutputStream(Files.newOutputStream(jar))) {
            out.putNextEntry(new JarEntry("webjars/lib/1.0/"));
            out.closeEntry();
            out.putNextEntry(new JarEntry("webjars/lib/1.0/a.txt"));
            out.write("a".getBytes());
            out.closeEntry();
        }
        List<String> got = new ArrayList<>();
        try (ZipFile zip = new ZipFile(jar.toFile())) {
            for (String name : List.of("webjars/lib/1.0", "webjars/lib/1.0/", "webjars/lib/1.0/a.txt", "webjars/lib"))
                got.add(name + "=" + (zip.getEntry(name) == null ? null : zip.getEntry(name).getName()));
        }
        try (URLClassLoader loader = new URLClassLoader(new URL[] { jar.toUri().toURL() }, null)) {
            for (String name : List.of("webjars/lib/1.0", "webjars/lib/1.0/", "webjars/lib/2.0"))
                got.add(name + ":" + (loader.getResource(name) != null));
        }
        Files.delete(jar);
        String expected = "[webjars/lib/1.0=webjars/lib/1.0/, webjars/lib/1.0/=webjars/lib/1.0/, "
            + "webjars/lib/1.0/a.txt=webjars/lib/1.0/a.txt, webjars/lib=null, "
            + "webjars/lib/1.0:true, webjars/lib/1.0/:true, webjars/lib/2.0:false]";
        System.out.println(expected.equals(got.toString()) ? "OK" : got.toString());
    }
}
