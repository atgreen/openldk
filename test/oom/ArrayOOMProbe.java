// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.lang.reflect.Array;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicReference;

public class ArrayOOMProbe {
    interface Maker { Object make(int size); }
    static volatile Object sink;
    static volatile OutOfMemoryError lastError;
    static Maker maker(String kind) {
        return switch (kind) {
            case "primitive" -> n -> new int[n];
            case "reference" -> n -> new Object[n];
            case "reflect-primitive" -> n -> Array.newInstance(long.class, n);
            case "reflect-reference" -> n -> Array.newInstance(Object.class, n);
            case "multi" -> n -> new byte[4][n];
            case "reflect-multi" -> n -> Array.newInstance(int.class, 4, n);
            default -> throw new AssertionError(kind);
        };
    }
    static void check(boolean value) { if (!value) throw new AssertionError(); }
    static void smallAllocations() {
        check(new boolean[32][0].length == 32);
        check(new byte[1024][0].length == 1024);
        check(new short[31][7][0].length == 31);
        check(new char[64][0].length == 64);
        check(new int[64][0].length == 64);
        check(new long[64][0].length == 64);
        check(new float[64][0].length == 64);
        check(new double[64][0].length == 64);
        int[][] numbers = new int[3][4];
        check(numbers[2][3] == 0);
        check(new Object[64][0].length == 64);
        Object[][] refs = (Object[][]) Array.newInstance(Object.class, 3, 4);
        check(refs[2][3] == null);
        check(((long[][])Array.newInstance(long.class, 2, 5))[1][4] == 0);
        check(new int[0][Integer.MAX_VALUE].length == 0);
        try { sink = new int[0][-1]; throw new AssertionError(); }
        catch (NegativeArraySizeException expected) { }
        try { sink = Array.newInstance(int.class, 0, -1); throw new AssertionError(); }
        catch (NegativeArraySizeException expected) { }
    }
    static void oversized() {
        Maker[] makers = {maker("primitive"), maker("reference"), maker("reflect-primitive"),
                          maker("reflect-reference"), maker("multi"), maker("reflect-multi"),
                          n -> new int[n][0], n -> Array.newInstance(int.class, n, 0)};
        for (Maker maker : makers) {
            try { sink = maker.make(Integer.MAX_VALUE); throw new AssertionError("allocation succeeded"); }
            catch (OutOfMemoryError expected) { lastError = expected; }
        }
    }
    static int fill(Maker maker) {
        Object[] retained = new Object[4096];
        int count = 0;
        try {
            for (; count < retained.length; count++) retained[count] = maker.make(1 << 18);
        } catch (OutOfMemoryError expected) { lastError = expected; }
        check(count < retained.length);
        return count;
    }
    static void concurrent() throws Exception {
        CountDownLatch start = new CountDownLatch(1);
        AtomicReference<Throwable> failure = new AtomicReference<>();
        Thread[] threads = new Thread[4];
        Maker maker = maker("reference");
        sink = maker.make(8);
        for (int i = 0; i < threads.length; i++) {
            threads[i] = new Thread(() -> {
                try { start.await(); fill(maker); }
                catch (Throwable error) { failure.set(error); }
            });
        }
        for (Thread thread : threads) thread.start();
        start.countDown();
        for (Thread thread : threads) {
            thread.join(60000);
            check(!thread.isAlive());
        }
        if (failure.get() != null) throw new AssertionError(failure.get());
    }
    public static void main(String[] args) throws Exception {
        if (Runtime.getRuntime().maxMemory() > (1L << 30))
            throw new IllegalArgumentException("Use a test runtime with at most a 1 GiB heap");
        smallAllocations();
        if (args[0].equals("oversized")) oversized();
        else if (args[0].equals("concurrent")) concurrent();
        else {
            Maker maker = maker(args[0]);
            sink = maker.make(8);
            for (int i = 0; i < 3; i++) { check(fill(maker) > 0); System.gc(); smallAllocations(); }
        }
        check(lastError != null);
        System.gc();
        smallAllocations();
        check(lastError instanceof VirtualMachineError);
        System.out.println(args[0] + ": caught OutOfMemoryError and recovered");
    }
}
