/* { dg-output "OK" } */

package aaa;

import java.io.*;
import java.nio.file.*;
import java.util.*;
import java.util.jar.JarFile;

// Opening a directory as a file fails as a JVM's does (Tomcat's jar
// scanner relies on it for directory classpath entries), a file: URL for
// a directory lists it, and a relative File resolves against user.dir.
public class fileopen
{
    interface Open { Closeable open() throws IOException; }

    static String attempt(Open open)
    {
        try (Closeable c = open.open()) {
            return "opened";
        } catch (IOException e) {
            return e.getClass().getSimpleName() + ": " + e.getMessage();
        }
    }

    public static void main(String[] args) throws Exception
    {
        Path base = Files.createTempDirectory("fileopen");
        Path dir = Files.createDirectory(base.resolve("dir"));
        Files.createDirectory(dir.resolve("sub"));
        Files.writeString(dir.resolve("b.txt"), "b");
        Files.writeString(dir.resolve("a.txt"), "a");
        String d = dir.toString();

        List<String> got = new ArrayList<>();
        got.add(attempt(() -> new FileInputStream(d)));
        got.add(attempt(() -> new RandomAccessFile(d, "r")));
        got.add(attempt(() -> new JarFile(d)));
        got.add(attempt(() -> new FileInputStream(d + "/missing")));
        try (InputStream in = dir.toUri().toURL().openStream()) {
            got.add(new String(in.readAllBytes()).replace('\n', ' ').trim());
        }
        File relative = new File("aaa");
        got.add(String.valueOf(relative.getAbsolutePath().equals(System.getProperty("user.dir") + File.separator + "aaa")));
        got.add(String.valueOf(relative.isDirectory() == new File(System.getProperty("user.dir"), "aaa").isDirectory()));

        String expected = "[FileNotFoundException: " + d + " (Is a directory), "
            + "FileNotFoundException: " + d + " (Is a directory), "
            + "FileNotFoundException: " + d + " (Is a directory), "
            + "FileNotFoundException: " + d + "/missing (No such file or directory), "
            + "a.txt b.txt sub, true, true]";
        System.out.println(expected.equals(got.toString()) ? "OK" : got.toString());
    }
}
