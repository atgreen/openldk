// Copyright (C) 2026 Anthony Green <green@moxielogic.com>
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.io.*;
import java.util.concurrent.atomic.AtomicReference;

public class MainThreadProbe {
    static void check(boolean value, String message) {
        if (!value) throw new AssertionError(message);
    }
    static void join(Thread worker, AtomicReference<Throwable> failure) throws Exception {
        worker.join(10000);
        check(!worker.isAlive(), "writer did not terminate");
        if (failure.get() != null) throw new AssertionError("writer failed", failure.get());
    }
    static void chars(Thread owner) throws Exception {
        PipedReader reader = new PipedReader();
        PipedWriter writer = new PipedWriter(reader);
        writer.write('!'); check(reader.read() == '!', "initial character");
        AtomicReference<Throwable> failure = new AtomicReference<>();
        Thread worker = new Thread(() -> {
            try (writer) {
                check(owner.isAlive(), "main thread looks dead to character writer");
                for (int i = 0; i < 4096; i++) writer.write('a' + i % 26);
            } catch (Throwable error) { failure.set(error); }
        });
        worker.start();
        int count = 0;
        try (reader) {
            for (int c; (c = reader.read()) != -1; count++)
                check(c == 'a' + count % 26, "character pipe data");
        }
        join(worker, failure);
        check(count == 4096, "character pipe length");
    }
    static void bytes(Thread owner) throws Exception {
        PipedInputStream reader = new PipedInputStream();
        PipedOutputStream writer = new PipedOutputStream(reader);
        writer.write(1); check(reader.read() == 1, "initial byte");
        AtomicReference<Throwable> failure = new AtomicReference<>();
        Thread worker = new Thread(() -> {
            try (writer) {
                check(owner.isAlive(), "main thread looks dead to byte writer");
                for (int i = 0; i < 4096; i++) writer.write(i & 255);
            } catch (Throwable error) { failure.set(error); }
        });
        worker.start();
        int count = 0;
        try (reader) {
            for (int b; (b = reader.read()) != -1; count++)
                check(b == (count & 255), "byte pipe data");
        }
        join(worker, failure);
        check(count == 4096, "byte pipe length");
    }
    public static void main(String[] args) throws Exception {
        Thread owner = Thread.currentThread();
        check(owner.isAlive(), "main thread is not alive");
        check(owner.threadId() > 0 && owner.getName().equals("main"), "main thread metadata");
        check(owner.getPriority() == Thread.NORM_PRIORITY && !owner.isDaemon()
              && owner.getThreadGroup() != null, "main thread attributes");
        check(owner.getState() != Thread.State.NEW && owner.getState() != Thread.State.TERMINATED,
              "main thread state is not running");
        chars(owner); bytes(owner);
        check(Thread.currentThread() == owner && owner.isAlive(), "main identity or lifetime changed");
        System.out.println("Main thread liveness and character/byte pipes passed");
    }
}
