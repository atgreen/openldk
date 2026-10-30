// Copyright (C) 2026 Anthony Green <green@moxielogic.com>
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.io.*;
import java.nio.file.*;
import java.util.*;
import java.util.zip.*;

public class ZipFailureProbe {
    interface Operation { void run() throws Exception; }
    static void fails(Class<? extends Throwable> expected, Operation operation) throws Exception {
        try { operation.run(); throw new AssertionError("missing " + expected.getName()); }
        catch (Throwable error) {
            if (!expected.isInstance(error)) throw new AssertionError("expected " + expected.getName(), error);
        }
    }
    static void check(boolean value, String message) { if (!value) throw new AssertionError(message); }
    static int at(byte[] data, int signature) {
        for (int i = 0; i <= data.length - 4; i++)
            if (word(data, i) == (signature & 0xffffffffL)) return i;
        throw new AssertionError("missing ZIP record");
    }
    static long word(byte[] data, int p) {
        return (data[p]&255L) | ((data[p+1]&255L)<<8) | ((data[p+2]&255L)<<16) | ((data[p+3]&255L)<<24);
    }
    static void put(byte[] data, int p, long value, int width) {
        for (int i = 0; i < width; i++) data[p+i] = (byte)(value >>> (8*i));
    }
    static byte[] archive(boolean empty, String comment) throws Exception {
        ByteArrayOutputStream bytes = new ByteArrayOutputStream();
        try (ZipOutputStream output = new ZipOutputStream(bytes)) {
            if (comment != null) output.setComment(comment);
            if (!empty) { output.putNextEntry(new ZipEntry("entry.txt")); output.write(new byte[] {3,1,4,1,5}); }
        }
        return bytes.toByteArray();
    }
    static byte[] zip64(byte[] good, boolean sentinels) {
        int end = at(good, 0x06054b50);
        byte[] data = new byte[good.length + 76];
        System.arraycopy(good, 0, data, 0, end);
        put(data, end, 0x06064b50, 4); put(data, end + 4, 44, 8);
        put(data, end + 12, 45, 2); put(data, end + 14, 45, 2);
        put(data, end + 24, 1, 8); put(data, end + 32, 1, 8);
        put(data, end + 40, word(good, end + 12), 8);
        put(data, end + 48, word(good, end + 16), 8);
        put(data, end + 56, 0x07064b50, 4); put(data, end + 64, end, 8);
        put(data, end + 72, 1, 4);
        System.arraycopy(good, end, data, end + 76, good.length - end);
        if (sentinels) {
            put(data, end + 76 + 8, 65535, 2); put(data, end + 76 + 10, 65535, 2);
            put(data, end + 76 + 12, 0xffffffffL, 4); put(data, end + 76 + 16, 0xffffffffL, 4);
        }
        return data;
    }
    static byte[] zip64Entry(byte[] good) {
        int central = at(good, 0x02014b50), end = at(good, 0x06054b50);
        int extra = central + 46 + 9;
        byte[] data = new byte[good.length + 28];
        System.arraycopy(good, 0, data, 0, extra);
        put(data, extra, 1, 2); put(data, extra + 2, 24, 2);
        put(data, extra + 4, word(good, central + 24), 8);
        put(data, extra + 12, word(good, central + 20), 8);
        put(data, extra + 20, word(good, central + 42), 8);
        System.arraycopy(good, extra, data, extra + 28, good.length - extra);
        put(data, central + 6, 45, 2); put(data, central + 30, 28, 2);
        put(data, central + 20, 0xffffffffL, 4); put(data, central + 24, 0xffffffffL, 4);
        put(data, central + 42, 0xffffffffL, 4);
        put(data, end + 28 + 12, word(good, end + 12) + 28, 4);
        return data;
    }
    static void valid(Path path, byte[] data, boolean empty) throws Exception {
        Files.write(path, data);
        try (ZipFile zip = new ZipFile(path.toFile())) {
            check(zip.size() == (empty ? 0 : 1), "entry count");
            if (!empty) try (InputStream input = zip.getInputStream(zip.getEntry("entry.txt"))) {
                check(Arrays.equals(input.readAllBytes(), new byte[] {3,1,4,1,5}), "entry content");
            }
        }
    }
    static void invalid(Path path, byte[] data) throws Exception {
        Files.write(path, data);
        fails(ZipException.class, () -> { try (ZipFile ignored = new ZipFile(path.toFile())) { } });
    }
    public static void main(String[] args) throws Exception {
        Path path = Files.createTempFile("openldk-zip-invalid", ".zip");
        try {
            byte[] good = archive(false, null);
            int end = at(good, 0x06054b50), central = at(good, 0x02014b50);
            valid(path, good, false); valid(path, archive(true, null), true);
            valid(path, zip64(good, false), false); valid(path, zip64(good, true), false);
            valid(path, zip64Entry(good), false); valid(path, zip64(zip64Entry(good), true), false);
            valid(path, archive(false, "comment PK\u0005\u0006 followed by more than twenty-two bytes of comment"), false);
            byte[] prefix = new byte[good.length + 17];
            System.arraycopy(good, 0, prefix, 17, good.length); valid(path, prefix, false);
            valid(path, Arrays.copyOf(good, good.length + 16), false);
            invalid(path, new byte[0]); invalid(path, new byte[] {0,1,2,3,4});
            invalid(path, Arrays.copyOfRange(good, end, end + 4));
            invalid(path, Arrays.copyOf(good, end + 10));
            byte[] bad = good.clone(); put(bad, end + 12, 0xffffffffL, 4); invalid(path, bad);
            bad = good.clone(); put(bad, end + 16, good.length + 1, 4); invalid(path, bad);
            bad = good.clone(); bad[central] = 0; invalid(path, bad);
            bad = good.clone(); put(bad, central + 28, 65535, 2); invalid(path, bad);
            bad = good.clone(); put(bad, central + 30, 65535, 2); invalid(path, bad);
            bad = good.clone(); put(bad, central + 10, 99, 2); invalid(path, bad);
            bad = good.clone(); bad[central + 8] |= 1; invalid(path, bad);
            bad = good.clone(); bad[central + 46] = (byte)0xff; invalid(path, bad);
            bad = good.clone(); put(bad, central + 24, 0xffffffffL, 4); invalid(path, bad);
            bad = zip64Entry(good); put(bad, central + 46 + 9 + 2, 1, 2); invalid(path, bad);
            fails(NullPointerException.class, () -> new ZipFile((String)null));
            fails(NullPointerException.class, () -> new ZipFile((File)null));
            Files.delete(path);
            fails(NoSuchFileException.class, () -> new ZipFile(path.toFile()));
            // Repeated failed parses must release descriptors immediately.
            for (int i = 0; i < 160; i++) invalid(path, new byte[] {1,2,3});
            valid(path, good, false);
            System.out.println("Malformed ZIP/ZIP64 records, Java exceptions, comments, archive stubs, and failed-open cleanup passed");
        } finally { Files.deleteIfExists(path); }
    }
}
