// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.net.URI;
import java.nio.file.*;
import java.util.Map;

public class JrtFileSystemProbe {
    public static void main(String[] args) throws Exception {
        try (FileSystem fs = FileSystems.newFileSystem(URI.create("jrt:/"),
                Map.of("java.home", System.getProperty("java.home")))) {
            Path object = fs.getPath("/modules/java.base/java/lang/Object.class");
            byte[] bytes = Files.readAllBytes(object);
            if (bytes.length < 100 || bytes[0] != (byte) 0xca || bytes[1] != (byte) 0xfe
                    || bytes[2] != (byte) 0xba || bytes[3] != (byte) 0xbe)
                throw new AssertionError("Runtime image class bytes");
            try (var modules = Files.list(fs.getPath("/modules"))) {
                if (modules.count() < 10) throw new AssertionError("Runtime image modules");
            }
        }
        System.out.println("Target-JDK JRT filesystem, class bytes, and module listing passed");
    }
}
