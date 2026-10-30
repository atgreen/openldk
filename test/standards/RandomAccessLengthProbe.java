// Copyright (C) 2026 Anthony Green <green@moxielogic.com>
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.io.*;
import java.nio.*;
import java.nio.channels.*;
import java.nio.file.*;

public class RandomAccessLengthProbe {
    interface Operation { void run() throws Exception; }
    static void check(long actual, long expected, String message) {
        if (actual != expected) throw new AssertionError(message + ": " + actual + " expected " + expected);
    }
    static void fails(Operation operation) throws Exception {
        try { operation.run(); throw new AssertionError("missing IOException"); }
        catch (IOException expected) { }
    }
    public static void main(String[] args) throws Exception {
        Path path = Files.createTempFile("openldk-resize", ".dat");
        try {
            try (RandomAccessFile file = new RandomAccessFile(path.toFile(), "rw")) {
                file.write(new byte[] {1,2,3,4,5,6,7,8});
                file.setLength(3);
                check(file.length(), 3, "flush pending writes before truncate");
                check(file.getFilePointer(), 3, "clamp pointer after truncate");
                check(file.read(), -1, "EOF after truncate");
                file.setLength(12);
                check(file.getFilePointer(), 3, "extension preserves position");
                check(file.length(), 12, "extension length");
                file.seek(30); file.setLength(12);
                check(file.getFilePointer(), 12, "unchanged length clamps outlying pointer");
                file.seek(0); check(file.read(), 1, "retained prefix");
                file.setLength(2);
                check(file.read(), 2, "read buffered prefix after truncation");
                check(file.read(), -1, "discard buffered input beyond truncated EOF");
                file.setLength(10); file.seek(0); file.read();
                file.setLength(0); check(file.getFilePointer(), 0, "zero truncation");
                check(file.read(), -1, "zero truncation clears input buffer");
                fails(() -> file.setLength(-1));
                check(file.length(), 0, "negative size preserves length");
                check(file.getFilePointer(), 0, "negative size preserves pointer");
                FileChannel channel = file.getChannel();
                file.write(new byte[] {9,8,7,6,5});
                file.setLength(3);
                check(channel.size(), 3, "channel observes truncated length");
                check(channel.position(), 3, "channel observes clamped pointer");
                channel.position(1); file.setLength(8);
                check(file.getFilePointer(), 1, "channel position survives extension");
                check(channel.read(ByteBuffer.allocate(2)), 2, "channel reads resized file");
                check(file.getFilePointer(), 3, "RAF sees channel read position");
                long large = (1L << 31) + 4097;
                file.setLength(large);
                check(file.length(), large, "64-bit sparse extension");
                file.seek(large - 1); file.write(91);
                file.setLength(large - 2);
                check(file.getFilePointer(), large - 2, "64-bit pointer clamp");
                file.setLength(0);
            }
            Files.write(path, new byte[] {1,2,3,4});
            try (RandomAccessFile file = new RandomAccessFile(path.toFile(), "r")) {
                file.seek(2); check(file.read(), 3, "read-only input before failure");
                fails(() -> file.setLength(0)); fails(() -> file.setLength(7));
                check(file.length(), 4, "read-only failure preserves length");
                check(file.getFilePointer(), 3, "read-only failure preserves position");
                check(file.read(), 4, "read-only failure preserves input");
            }
            RandomAccessFile closed = new RandomAccessFile(path.toFile(), "rw"); closed.close();
            fails(() -> closed.setLength(0)); fails(() -> closed.setLength(-1));
            System.out.println("Random-access resize, buffered input/output, shared channels, errors, and 64-bit lengths passed");
        } finally { Files.deleteIfExists(path); }
    }
}
