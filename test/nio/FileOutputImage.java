// Copyright (C) 2026 Anthony Green <green@moxielogic.com>
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0

import java.io.*;
import java.nio.file.*;
import java.util.Arrays;

public class FileOutputImage {
    static FileOutputStream plain, alias, append;
    static RandomAccessFile random;
    static Path directory;
    static void check(boolean value) {
        if (!value) throw new AssertionError();
    }
    static void warm(String path) throws Exception {
        directory = Path.of(path);
        Files.createDirectories(directory);
        plain = new FileOutputStream(directory.resolve("plain.bin").toFile());
        alias = new FileOutputStream(plain.getFD());
        plain.write(new byte[] {1, 2, 3});
        plain.getChannel().position(1);
        Files.write(directory.resolve("append.bin"), new byte[] {4});
        append = new FileOutputStream(directory.resolve("append.bin").toFile(), true);
        append.getChannel().position(0);
        check(append.getChannel().position() == 1);
        Files.write(directory.resolve("random.bin"), new byte[] {10, 20, 30});
        random = new RandomAccessFile(directory.resolve("random.bin").toFile(), "rw");
        check(random.read() == 10);
        random.write(40); // Deliberately leave Lisp-backed RAF output pending.
    }
    static void resume() throws Exception {
        plain.write(8);
        alias.write(7);
        check(plain.getChannel().position() == 3);
        check(Arrays.equals(Files.readAllBytes(directory.resolve("plain.bin")), new byte[] {1, 8, 7}));
        check(append.getChannel().position() == 1);
        append.write(5);
        check(Arrays.equals(Files.readAllBytes(directory.resolve("append.bin")), new byte[] {4, 5}));
        check(random.read() == 30 && random.read() == -1);
        check(Arrays.equals(Files.readAllBytes(directory.resolve("random.bin")), new byte[] {10, 40, 30}));
        alias.close();
        plain.close();
        append.close();
        random.close();
        System.out.println("File-output image flags, positions, sharing and visibility passed");
    }
    public static void main(String[] args) throws Exception {
        if (args.length > 0 && args[0].equals("warm")) {
            warm(args[1]);
        } else {
            if (args.length > 0 && args[0].equals("roundtrip")) warm(args[1]);
            resume();
        }
    }
}
