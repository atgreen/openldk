// Copyright (C) 2026 Anthony Green <green@moxielogic.com>
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.io.*;
import java.nio.*;
import java.nio.channels.*;
import java.nio.file.*;
import java.util.*;

public class FileDescriptorCloseProbe {
    static void check(boolean value, String message) {
        if (!value) throw new AssertionError(message);
    }
    static void transfer(Path path) throws Exception {
        Files.write(path, new byte[] {1,2,3});
        check(Arrays.equals(Files.readAllBytes(path), new byte[] {1,2,3}), "NIO round trip");
        FileChannel closed = FileChannel.open(path, StandardOpenOption.READ);
        closed.close();
        try (FileChannel live = FileChannel.open(path, StandardOpenOption.READ)) {
            closed.close();
            ByteBuffer buffer = ByteBuffer.allocate(3);
            check(live.read(buffer) == 3 && buffer.get(0) == 1, "repeated close affected reused descriptor");
        }
    }
    static int descriptors() {
        String[] names = new File("/proc/self/fd").list();
        return names == null ? -1 : names.length;
    }
    public static void main(String[] args) throws Exception {
        Path path = Files.createTempFile("openldk-file-close", ".dat");
        try {
            // Warm all paths before counting resources retained by the loop.
            for (int i = 0; i < 3; i++) transfer(path);
            int before = descriptors();
            for (int i = 0; i < 160; i++) transfer(path);
            int after = descriptors();
            check(before < 0 || after <= before + 2, "raw NIO descriptors leaked: " + before + " -> " + after);
            FileOutputStream output = new FileOutputStream(path.toFile());
            FileDescriptor descriptor = output.getFD();
            check(descriptor.valid(), "new stream descriptor invalid");
            output.close(); output.close();
            check(!descriptor.valid(), "shared descriptor remains valid after close");
            System.out.println("NIO descriptor release, shared descriptor invalidation, and safe repeated closes passed");
        } finally { Files.deleteIfExists(path); }
    }
}
