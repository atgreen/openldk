// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.io.*;
import java.nio.file.*;
import java.util.Arrays;

public class FileInputProbe {
    public static void main(String[] args) throws Exception {
        Path path = Path.of(args[0]);
        byte[] data = new byte[65557];
        for (int i = 0; i < data.length; i++) data[i] = (byte) (i * 37);
        Files.write(path, data);
        try {
            try (FileInputStream input = new FileInputStream(path.toFile())) {
                if (!Arrays.equals(input.readNBytes(19), Arrays.copyOfRange(data, 0, 19)))
                    throw new AssertionError("Prefix read");
                if (input.skip(7) != 7 || input.skip(-11) != -11)
                    throw new AssertionError("Regular-file seek");
                if (!Arrays.equals(input.readAllBytes(), Arrays.copyOfRange(data, 15, data.length)))
                    throw new AssertionError("Bulk read after buffered input and seeks");
                if (input.readAllBytes().length != 0) throw new AssertionError("EOF read");
            }
            FileInputStream closed = new FileInputStream(path.toFile());
            closed.readNBytes(1);
            closed.close();
            try { closed.readAllBytes(); throw new AssertionError("Closed stream read succeeded"); }
            catch (IOException expected) { }
            try (FileInputStream invalid = new FileInputStream(new FileDescriptor())) {
                try { invalid.readAllBytes(); throw new AssertionError("Invalid descriptor read succeeded"); }
                catch (IOException expected) { }
            }
            try (FileInputStream special = new FileInputStream("/dev/null")) {
                if (special.readAllBytes().length != 0) throw new AssertionError("Non-regular file read");
            }
        } finally { Files.delete(path); }
        for (String name : new String[] {"native[range]*?.bin", "native\\backslash.bin"}) {
            Path literal = path.resolveSibling(name);
            try {
                try (FileOutputStream output = new FileOutputStream(literal.toFile())) {
                    output.write(new byte[] {1, 2, 3});
                }
                try (FileOutputStream output = new FileOutputStream(literal.toFile(), true)) {
                    output.write(4);
                }
                try (FileInputStream input = new FileInputStream(literal.toFile())) {
                    if (!Arrays.equals(input.readAllBytes(), new byte[] {1, 2, 3, 4}))
                        throw new AssertionError("Literal filename stream access");
                }
                try (RandomAccessFile file = new RandomAccessFile(literal.toFile(), "rw")) {
                    file.seek(1);
                    file.write(9);
                    file.seek(0);
                    if (file.read() != 1 || file.read() != 9 || file.length() != 4)
                        throw new AssertionError("Literal filename random access");
                }
            } finally { Files.deleteIfExists(literal); }
        }
        System.out.println("FileInputStream bulk reads, positions, seeks, descriptors, and special files passed");
    }
}
