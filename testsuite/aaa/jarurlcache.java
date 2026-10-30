/* { dg-output "OK" } */
/* Copyright (C) 2026 Anthony Green
 * SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0 */
package aaa;
import java.io.*;
import java.net.*;
import java.util.jar.*;
public class jarurlcache {
    static void check(boolean b) { if (!b) throw new AssertionError(); }
    static JarURLConnection connection(URL u, boolean cache) throws Exception {
        JarURLConnection c = (JarURLConnection) u.openConnection();
        c.setUseCaches(cache); return c;
    }
    public static void main(String[] args) throws Exception {
        File f = File.createTempFile("jar cache+", ".jar");
        try {
            try (JarOutputStream out = new JarOutputStream(new FileOutputStream(f))) {
                out.putNextEntry(new JarEntry("entry +.txt")); out.write(42); out.closeEntry();
            }
            URL u = new URL("jar:" + f.toURI().toURL() + "!/entry%20+.txt");
            JarURLConnection a = connection(u, true), b = connection(u, true);
            JarFile shared = a.getJarFile(); check(shared == b.getJarFile());
            check(a.getJarEntry().getName().equals("entry +.txt"));
            boolean defaultCache = URLConnection.getDefaultUseCaches("jar");
            URLConnection.setDefaultUseCaches("jar", false);
            try { check(!u.openConnection().getUseCaches()); }
            finally { URLConnection.setDefaultUseCaches("jar", defaultCache); }
            try (InputStream in = a.getInputStream()) { check(in.read() == 42); }
            check(shared.size() == 1);
            JarURLConnection c = connection(u, false), d = connection(u, false);
            JarFile privateFile = c.getJarFile(); check(privateFile != d.getJarFile());
            c.getInputStream().close();
            try { privateFile.size(); throw new AssertionError(); }
            catch (IllegalStateException expected) {}
            d.getJarFile().close(); shared.close();
            JarFile replacement = connection(u, true).getJarFile();
            check(replacement != shared); replacement.close();
            URL root = new URL("jar:" + f.toURI().toURL() + "!/");
            JarURLConnection r = connection(root, false);
            check(r.getJarFile().size() == 1);
            try { r.getInputStream(); throw new AssertionError(); }
            catch (IOException expected) {}
            r.getJarFile().close();
            System.out.println("OK");
        } finally { f.delete(); }
    }
}
