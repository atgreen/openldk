/* { dg-output "OK" } */

package aaa;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.jar.*;
import java.util.zip.*;

// H2 reads its bundled data.zip with ZipInputStream.getNextEntry, and
// JarInputStream extends it: both run the JDK's code over Inflater.
public class zipstream
{
    static byte[] bytes(String s) { return s.getBytes(StandardCharsets.UTF_8); }

    static String readAll(InputStream in) throws IOException
    {
        return new String(in.readAllBytes(), StandardCharsets.UTF_8);
    }

    public static void main(String[] args) throws Exception
    {
        String big = "zip ".repeat(5000);
        ByteArrayOutputStream bout = new ByteArrayOutputStream();
        try (ZipOutputStream out = new ZipOutputStream(bout)) {
            out.putNextEntry(new ZipEntry("deflated.txt"));
            out.write(bytes(big));
            out.closeEntry();
            byte[] stored = bytes("stored");
            ZipEntry e = new ZipEntry("dir/stored.txt");
            e.setMethod(ZipEntry.STORED);
            e.setSize(stored.length);
            CRC32 crc = new CRC32();
            crc.update(stored);
            e.setCrc(crc.getValue());
            out.putNextEntry(e);
            out.write(stored);
            out.closeEntry();
        }

        List<String> got = new ArrayList<>();
        try (ZipInputStream in = new ZipInputStream(new ByteArrayInputStream(bout.toByteArray()))) {
            for (ZipEntry e; (e = in.getNextEntry()) != null; ) {
                String text = readAll(in);
                got.add(e.getName() + ":" + text.length() + ":" + text.equals(e.getName().startsWith("dir") ? "stored" : big));
            }
        }

        Manifest man = new Manifest();
        man.getMainAttributes().put(Attributes.Name.MANIFEST_VERSION, "1.0");
        man.getMainAttributes().put(Attributes.Name.MAIN_CLASS, "demo.Main");
        ByteArrayOutputStream jout = new ByteArrayOutputStream();
        try (JarOutputStream out = new JarOutputStream(jout, man)) {
            out.putNextEntry(new JarEntry("demo/Main.class"));
            out.write(bytes("not really a class"));
            out.closeEntry();
        }
        try (JarInputStream in = new JarInputStream(new ByteArrayInputStream(jout.toByteArray()))) {
            got.add(in.getManifest().getMainAttributes().getValue("Main-Class"));
            for (JarEntry e; (e = in.getNextJarEntry()) != null; )
                got.add(e.getName() + ":" + readAll(in));
        }

        String expected = "[deflated.txt:20000:true, dir/stored.txt:6:true, demo.Main, "
            + "demo/Main.class:not really a class]";
        System.out.println(expected.equals(got.toString()) ? "OK" : got.toString());
    }
}
