// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.net.URL;
import java.net.URLClassLoader;
import java.io.File;
import java.util.jar.JarFile;
import java.util.jar.JarEntry;
import java.util.zip.CRC32;

public class ZipMetadataProbe {
    public static void main(String[] args) throws Exception {
        boolean corrupt = args.length > 1;
        try (JarFile file = new JarFile(args[0])) {
            var entries = file.entries();
            int count = 0;
            while (entries.hasMoreElements()) {
                JarEntry entry = entries.nextElement();
                byte[] bytes = file.getInputStream(entry).readAllBytes();
                CRC32 crc = new CRC32();
                crc.update(bytes);
                long expected = crc.getValue();
                if (corrupt && entry.getName().equals("ZipPayload.class")) expected ^= 1;
                if (entry.getCrc() != expected)
                    throw new AssertionError(entry.getName() + ": " + entry.getCrc() + " != " + expected);
                int method = entry.getName().equals("stored.bin") ? 0 : 8;
                if (entry.getMethod() != method) throw new AssertionError("Compression method");
                if (file.getJarEntry(entry.getName()).getCrc() != expected)
                    throw new AssertionError("Lookup and enumeration disagree");
                count++;
            }
            if (count != 8) throw new AssertionError("Entry count: " + count);
        }
        try (URLClassLoader loader = new URLClassLoader(new URL[]{new File(args[0]).toURI().toURL()}, null)) {
            // JDK 25 records CRC errors for ClassFormatError diagnostics;
            // a valid class still loads even when its stored CRC is wrong.
            Class<?> cls = loader.loadClass("ZipPayload");
            if (!Integer.valueOf(42).equals(cls.getMethod("answer").invoke(null)))
                throw new AssertionError("Loaded class result");
        }
        System.out.println("ZIP metadata and class loading passed: " + (corrupt ? "corrupt" : "valid"));
    }
}
