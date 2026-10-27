// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
public class StringInternProbe {
    private static volatile boolean go;
    private static volatile Throwable failure;
    public static void main(String[] args) throws Exception {
        int workers = 8, count = 2000;
        String[][] values = new String[workers][count];
        boolean[] finished = new boolean[workers];
        Thread[] threads = new Thread[workers];
        for (int i = 0; i < workers; i++) {
            final int worker = i;
            threads[i] = new Thread(() -> {
                try {
                    while (!go) Thread.yield();
                    for (int j = 0; j < count; j++) {
                        values[worker][j] = new String("intern-common-" + j).intern();
                        new String("intern-unique-" + worker + "-" + j).intern();
                    }
                    finished[worker] = true;
                } catch (Throwable error) { failure = error; }
            });
            threads[i].start();
        }
        go = true;
        for (Thread thread : threads) thread.join();
        if (failure != null) throw new AssertionError("Intern worker failed", failure);
        for (int worker = 0; worker < workers; worker++) {
            if (!finished[worker]) throw new AssertionError("Intern worker did not finish");
            for (int j = 0; j < count; j++) {
                if (values[worker][j] != values[0][j])
                    throw new AssertionError("Equal interned strings have different identities");
            }
        }
        System.out.println("Concurrent string interning preserves canonical identity");
    }
}
