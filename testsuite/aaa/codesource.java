/* { dg-output "OK" } */

package aaa;

import java.lang.reflect.Field;
import java.net.InetAddress;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.jar.JarEntry;
import java.util.jar.JarOutputStream;

// Runtime services Quarkus's startup uses: a class's code source (its
// launcher finds its own jar so), compare-and-set on a static field through
// Unsafe.staticFieldBase, the jar: file system (SmallRye Config reads
// application.properties through it), and InetAddress's initialization.
public class codesource
{
    static int counter;

    public static void main(String[] args) throws Exception
    {
        List<String> got = new ArrayList<>();
        java.security.CodeSource cs = codesource.class.getProtectionDomain().getCodeSource();
        got.add(String.valueOf(cs != null && cs.getLocation().getProtocol().equals("file")));
        got.add(String.valueOf(String.class.getProtectionDomain().getCodeSource()));

        Field theUnsafe = sun.misc.Unsafe.class.getDeclaredField("theUnsafe");
        theUnsafe.setAccessible(true);
        sun.misc.Unsafe unsafe = (sun.misc.Unsafe) theUnsafe.get(null);
        Field field = codesource.class.getDeclaredField("counter");
        Object base = unsafe.staticFieldBase(field);
        long offset = unsafe.staticFieldOffset(field);
        got.add(String.valueOf(unsafe.compareAndSwapInt(base, offset, 0, 5)));
        got.add(String.valueOf(counter));

        Path jar = Files.createTempFile("codesource", ".jar");
        try (JarOutputStream out = new JarOutputStream(Files.newOutputStream(jar))) {
            out.putNextEntry(new JarEntry("a/b.txt"));
            out.write("hello".getBytes());
            out.closeEntry();
        }
        try (FileSystem fs = FileSystems.newFileSystem(jar)) {
            got.add(Files.readString(fs.getPath("a/b.txt")));
        }
        Files.delete(jar);

        got.add(InetAddress.getByName("127.0.0.1").getHostAddress());
        got.add(InetAddress.getByAddress(new byte[16]).getClass().getSimpleName());

        String expected = "[true, null, true, 5, hello, 127.0.0.1, Inet6Address]";
        System.out.println(expected.equals(got.toString()) ? "OK" : got.toString());
    }
}
