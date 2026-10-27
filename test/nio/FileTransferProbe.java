// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.io.*;
import java.nio.file.*;
import java.util.Arrays;

public class FileTransferProbe {
    public static void main(String[] args) throws Exception {
        Path source = Path.of(args[0]), target = source.resolveSibling("transfer-target.bin");
        byte[] data = new byte[65557];
        for (int i = 0; i < data.length; i++) data[i] = (byte) (i * 37);
        Files.write(source, data);
        try {
            try (FileInputStream input = new FileInputStream(source.toFile());
                 FileOutputStream output = new FileOutputStream(target.toFile())) {
                output.write(new byte[]{9, 8, 7});
                var in = input.getChannel();
                var out = output.getChannel();
                in.position(17);
                long count = in.transferTo(19, 60000, out);
                if (count != 60000 || in.position() != 17 || out.position() != 60003)
                    throw new AssertionError("Positioned transfer counts or positions");
                if (in.transferTo(data.length - 7, 100, out) != 7
                        || in.transferTo(data.length + 3, 100, out) != 0)
                    throw new AssertionError("Transfer EOF");
            }
            byte[] expected = new byte[60010];
            expected[0] = 9; expected[1] = 8; expected[2] = 7;
            System.arraycopy(data, 19, expected, 3, 60000);
            System.arraycopy(data, data.length - 7, expected, 60003, 7);
            if (!Arrays.equals(expected, Files.readAllBytes(target)))
                throw new AssertionError("Transferred file contents");
            try (FileInputStream input = new FileInputStream(source.toFile());
                 FileOutputStream output = new FileOutputStream(target.toFile())) {
                input.readNBytes(11);
                long copied = input.transferTo(output);
                if (copied != data.length - 11)
                    throw new AssertionError("Stream transfer count: " + copied);
            }
            if (!Arrays.equals(Arrays.copyOfRange(data, 11, data.length), Files.readAllBytes(target)))
                throw new AssertionError("Stream transfer contents");
        } finally {
            Files.deleteIfExists(source);
            Files.deleteIfExists(target);
        }
        System.out.println("File transfers, buffered prefixes, positions, partial EOF, and stream copies passed");
    }
}
