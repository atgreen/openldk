// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.io.*;
import java.nio.charset.*;
import java.nio.file.*;
import java.util.*;

public class EncodingProbe {
    static void check(boolean value) { if (!value) throw new AssertionError(); }
    public static void main(String[] args) throws Exception {
        Charset expected = Charset.forName(args[0]);
        check(Charset.defaultCharset().equals(expected));
        String originalEncoding = System.setProperty("file.encoding", "US-ASCII");
        check(Charset.defaultCharset().equals(expected));
        System.setProperty("file.encoding", originalEncoding);
        String text = "caf\u00e9";
        byte[] encoded = text.getBytes(expected);
        check(Arrays.equals(text.getBytes(), encoded));
        check(new String(encoded).equals(text));
        var output = new ByteArrayOutputStream();
        try (var writer = new OutputStreamWriter(output)) { writer.write(text); }
        check(Arrays.equals(output.toByteArray(), encoded));
        try (var reader = new InputStreamReader(new ByteArrayInputStream(encoded))) {
            char[] chars = new char[20]; int count = reader.read(chars);
            check(new String(chars, 0, count).equals(text));
        }
        Path directory = Files.createTempDirectory("ldk-encoding-");
        Path file = directory.resolve("caf\u00e9-\u03bb.txt");
        try {
            try (var writer = new FileWriter(file.toFile())) { writer.write(text); }
            check(Arrays.equals(Files.readAllBytes(file), encoded));
            try (var reader = new FileReader(file.toFile())) {
                char[] chars = new char[20]; int count = reader.read(chars);
                check(new String(chars, 0, count).equals(text));
            }
            // Files convenience methods specify UTF-8 independently of the default charset.
            String unicode = "caf\u00e9 \u03bb \ud83d\ude80";
            Files.writeString(file, unicode);
            check(Arrays.equals(Files.readAllBytes(file), unicode.getBytes(StandardCharsets.UTF_8)));
            check(Files.readString(file).equals(unicode));
            try (var writer = new FileWriter(file.toFile(), StandardCharsets.UTF_16LE)) { writer.write(unicode); }
            try (var reader = new FileReader(file.toFile(), StandardCharsets.UTF_16LE)) {
                char[] chars = new char[40]; int count = reader.read(chars);
                check(new String(chars, 0, count).equals(unicode));
            }
        } finally { Files.deleteIfExists(file); Files.deleteIfExists(directory); }
        System.out.println("Encoding: " + expected.name() + " defaults, readers/writers, UTF-8 Files methods, and explicit charsets passed");
    }
}
