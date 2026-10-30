// Copyright (C) 2026 Anthony Green <green@moxielogic.com>
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0

import java.io.*;
import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.nio.file.*;
import java.nio.file.attribute.PosixFilePermission;
import java.util.*;

public class FileOutputProbe {
    interface Action { void run() throws Exception; }
    static int checks;
    static void check(boolean value, String message) {
        checks++;
        if (!value) throw new AssertionError(message);
    }
    static void expect(Class<? extends Throwable> type, Action action) throws Exception {
        try { action.run(); }
        catch (Throwable error) {
            check(type.isInstance(error), "expected " + type.getName() + ", got " + error);
            return;
        }
        throw new AssertionError("missing " + type.getName());
    }
    static void bytes(Path path, byte... expected) throws Exception {
        check(Arrays.equals(Files.readAllBytes(path), expected), "visible file contents");
    }
    public static void main(String[] args) throws Exception {
        Path path = Files.createTempFile("openldk-output-", ".bin");
        try {
            FileOutputStream output = new FileOutputStream(path.toFile());
            output.write(0x1ff);
            check(Files.size(path) == 1, "single write visible before flush/close");
            bytes(path, (byte) 255);
            output.write(new byte[] {8, -1, -128, 7}, 1, 2);
            bytes(path, (byte) 255, (byte) 255, (byte) 128);
            output.write(new byte[0]);
            output.write(new byte[] {4}, 1, 0);
            expect(NullPointerException.class, () -> output.write((byte[]) null));
            expect(NullPointerException.class, () -> output.write(null, -1, -1));
            expect(IndexOutOfBoundsException.class, () -> output.write(new byte[1], -1, 1));
            expect(IndexOutOfBoundsException.class, () -> output.write(new byte[1], 0, -1));
            expect(IndexOutOfBoundsException.class, () -> output.write(new byte[1], 1, 1));
            expect(IndexOutOfBoundsException.class, () -> output.write(new byte[1], Integer.MAX_VALUE, 2));
            bytes(path, (byte) 255, (byte) 255, (byte) 128);
            output.getFD().sync();
            check(output.getChannel().position() == 3, "channel shares stream position");
            output.getChannel().write(ByteBuffer.wrap(new byte[] {5}));
            output.write(6);
            bytes(path, (byte) 255, (byte) 255, (byte) 128, (byte) 5, (byte) 6);
            output.close();
            output.flush();
            output.write(new byte[0]);
            expect(IOException.class, () -> output.write(3));
            expect(IOException.class, () -> output.write(new byte[] {1}));
            expect(NullPointerException.class, () -> output.write((byte[]) null));
            expect(IndexOutOfBoundsException.class, () -> output.write(new byte[0], -1, 0));

            Files.write(path, new byte[] {1, 2, 3, 4});
            Set<PosixFilePermission> permissions = new HashSet<>(Arrays.asList(
                PosixFilePermission.OWNER_READ, PosixFilePermission.OWNER_WRITE,
                PosixFilePermission.GROUP_READ));
            Files.setPosixFilePermissions(path, permissions);
            try (FileInputStream before = new FileInputStream(path.toFile());
                 FileOutputStream truncate = new FileOutputStream(path.toFile())) {
                check(Files.size(path) == 0, "truncate immediately");
                truncate.write(9);
                check(before.read() == 9 && before.read() == -1, "truncate preserves inode");
                check(Files.getPosixFilePermissions(path).equals(permissions), "existing permissions retained");
            }
            Files.write(path, new byte[0]);
            try (FileOutputStream a = new FileOutputStream(path.toFile(), true);
                 FileOutputStream b = new FileOutputStream(path.toFile(), true)) {
                a.write(1);
                b.write(2);
                a.getChannel().position(0);
                check(a.getChannel().position() == 2, "append channel reports EOF after seek");
                a.write(new byte[] {3, 4});
                bytes(path, (byte) 1, (byte) 2, (byte) 3, (byte) 4);
            }
            try (FileOutputStream original = new FileOutputStream(path.toFile());
                 FileOutputStream alias = new FileOutputStream(original.getFD())) {
                original.write(1);
                alias.write(2);
                bytes(path, (byte) 1, (byte) 2);
                check(alias.getChannel().position() == 2, "shared descriptor position");
            }
            Files.write(path, new byte[] {1, 2, 3});
            try (RandomAccessFile random = new RandomAccessFile(path.toFile(), "rw");
                 FileOutputStream shared = new FileOutputStream(random.getFD())) {
                check(random.read() == 1, "random-access initial read");
                shared.write(9);
                check(random.getFilePointer() == 2, "read-ahead synchronized before shared write");
                check(random.read() == 3, "random-access read after shared output");
                bytes(path, (byte) 1, (byte) 9, (byte) 3);
            }
            try (ObjectOutputStream objects = new ObjectOutputStream(new FileOutputStream(path.toFile()))) {
                BigInteger value = new BigInteger("123456789012345678901234567890");
                objects.writeObject(value);
                try (ObjectInputStream input = new ObjectInputStream(new FileInputStream(path.toFile()))) {
                    check(input.readObject().equals(value), "serialization visible before flush/close");
                }
            }
            if (Files.exists(Path.of("/dev/full"))) {
                try (FileOutputStream full = new FileOutputStream("/dev/full")) {
                    expect(IOException.class, () -> full.write(1));
                    expect(IOException.class, () -> full.write(new byte[] {1, 2}));
                }
                checks++;
            }
        } finally {
            Files.deleteIfExists(path);
        }
        System.out.println("File-output visibility, ownership, errors and modes: " + checks + " checks passed");
    }
}
