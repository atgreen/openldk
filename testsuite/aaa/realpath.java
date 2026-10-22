/* { dg-output "OK" } */

package aaa;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;

// toRealPath and getCanonicalPath give a directory without a trailing
// slash, as realpath(3) does, so a file's real path starts with its
// directory's.  Both added a slash, and Kotlin's compiler, matching a
// class file to its classpath directory by real path, couldn't see the
// Kotlin classes in a directory.  Names with wildcard characters resolve.
public class realpath
{
    public static void main(String[] args) throws Exception
    {
        Path root = Files.createTempDirectory("realpath");
        Path dir = Files.createDirectory(root.resolve("a*b[c]"));
        Path file = Files.createFile(dir.resolve("A.class"));
        Path realDir = dir.toRealPath();
        String canonical = dir.toFile().getCanonicalPath();
        boolean ok = !realDir.toString().endsWith("/")
            && !canonical.endsWith("/")
            && realDir.toString().equals(canonical)
            && realDir.getFileName().toString().equals("a*b[c]")
            && file.toRealPath().startsWith(realDir)
            && file.toFile().getCanonicalPath().startsWith(canonical + "/")
            && new File(dir.toFile(), "missing").getCanonicalPath().equals(canonical + "/missing");
        Files.delete(file); Files.delete(dir); Files.delete(root);
        System.out.println(ok ? "OK" : "FAIL: " + realDir + " " + canonical);
    }
}
