/* { dg-output "OK" } */
/* Copyright (C) 2026 Anthony Green
 * SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0 */
package aaa;
import java.io.*;
import java.util.*;
import java.util.zip.*;
public class jarurlstream {
    static void check(boolean b) { if (!b) throw new AssertionError(); }
    public static void main(String[] args) throws Exception {
        File f = File.createTempFile("zip-stream", ".zip");
        byte[] data = new byte[200000];
        for (int i = 0; i < data.length; i++) data[i] = (byte)(i * 31);
        try {
            try (ZipOutputStream out = new ZipOutputStream(new FileOutputStream(f))) {
                for (int method : new int[]{0, 8}) {
                    ZipEntry e = new ZipEntry("entry" + method); e.setMethod(method);
                    if (method == 0) {
                        CRC32 crc = new CRC32(); crc.update(data);
                        e.setSize(data.length); e.setCrc(crc.getValue());
                    }
                    out.putNextEntry(e); out.write(data); out.closeEntry();
                }
            }
            for (int method : new int[]{0, 8}) {
                ZipFile z = new ZipFile(f);
                InputStream a = z.getInputStream(z.getEntry("entry" + method));
                InputStream b = z.getInputStream(z.getEntry("entry" + method));
                try { a.read(new byte[2], 1, 2); throw new AssertionError(); }
                catch (IndexOutOfBoundsException expected) {}
                try { a.read(null, 0, 1); throw new AssertionError(); }
                catch (NullPointerException expected) {}
                check(a.read() == (data[0] & 255));
                check(b.read() == (data[0] & 255));
                check(Arrays.equals(a.readAllBytes(), Arrays.copyOfRange(data, 1, data.length)));
                int empty = a.read(new byte[0], 0, 0);
                check(empty == 0 || empty == -1); check(a.read() == -1);
                z.close();
                try { b.read(); throw new AssertionError(); }
                catch (IOException expected) {}
                a.close(); b.close(); z.close();
            }
            System.out.println("OK");
        } finally { f.delete(); }
    }
}
