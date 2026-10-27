// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.util.*;
import java.util.concurrent.atomic.AtomicReference;
import java.util.jar.JarFile;

public class ZipConcurrentProbe {
    static volatile boolean start;
    public static void main(String[] args) throws Exception {
        try (JarFile jar = new JarFile(args[0])) {
            String[] names = {"stored.bin", "deflated.bin", "ZipPayload.class", "ZipPayload$Kind.class"};
            byte[][] expected = new byte[names.length][];
            for (int i = 0; i < names.length; i++)
                expected[i] = jar.getInputStream(jar.getJarEntry(names[i])).readAllBytes();
            AtomicReference<Throwable> failure = new AtomicReference<>();
            Thread[] workers = new Thread[8];
            boolean[] finished = new boolean[workers.length];
            for (int i = 0; i < workers.length; i++) {
                final int worker = i;
                workers[i] = new Thread(() -> {
                    try {
                        while (!start) Thread.yield();
                        for (int j = 0; j < 40; j++) {
                            int entry = (worker + j) % names.length;
                            try (var input = jar.getInputStream(jar.getJarEntry(names[entry]))) {
                                if (!Arrays.equals(expected[entry], input.readAllBytes()))
                                    throw new AssertionError("Concurrent ZIP read corrupted " + names[entry]);
                            }
                        }
                        finished[worker] = true;
                    } catch (Throwable error) { failure.compareAndSet(null, error); }
                });
                workers[i].start();
            }
            start = true;
            for (Thread worker : workers) worker.join();
            if (failure.get() != null) throw new AssertionError("Concurrent ZIP read failed", failure.get());
            for (boolean done : finished) if (!done) throw new AssertionError("ZIP reader terminated prematurely");
        }
        System.out.println("Concurrent ZIP reads passed");
    }
}
