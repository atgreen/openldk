/* { dg-output "OK" } */
/* Copyright (C) 2026 Anthony Green
 * SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0 */
package aaa;
import java.io.*;
import java.net.*;
import java.util.jar.*;
public class jarurlfds {
    static void abandon(File f) throws IOException {
        FileInputStream in = new FileInputStream(f);
        if (in.read() < 0) throw new AssertionError();
    }
    public static void main(String[] args) throws Exception {
        File f = File.createTempFile("jar-fds", ".jar");
        try {
            try (JarOutputStream out = new JarOutputStream(new FileOutputStream(f))) {
                out.putNextEntry(new JarEntry("entry")); out.write(7); out.closeEntry();
            }
            URL good = new URL("jar:" + f.toURI().toURL() + "!/entry");
            URL missing = new URL("jar:" + f.toURI().toURL() + "!/missing");
            for (int i = 0; i < 500; i++) {
                URLConnection c = good.openConnection(); c.setUseCaches(false);
                try (InputStream in = c.getInputStream()) {
                    if (in.read() != 7) throw new AssertionError();
                }
                c = missing.openConnection(); c.setUseCaches(false);
                try { c.getInputStream(); throw new AssertionError(); }
                catch (FileNotFoundException expected) {}
            }
            for (int i = 0; i < 500; i++) abandon(f);
            System.gc();
            System.out.println("OK");
        } finally { f.delete(); }
    }
}
