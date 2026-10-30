// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.lang.invoke.*;
import java.util.concurrent.atomic.AtomicReference;
import java.nio.ByteOrder;

public class MemoryModelProbe {
    int number;
    long wide;
    float single;
    double decimal;
    volatile int ready;
    int payload;
    final int immutable = 42;
    static int checks;
    static final VarHandle NUMBER, WIDE, SINGLE, DECIMAL, READY;
    static {
        try {
            var l = MethodHandles.lookup();
            NUMBER = l.findVarHandle(MemoryModelProbe.class, "number", int.class);
            WIDE = l.findVarHandle(MemoryModelProbe.class, "wide", long.class);
            SINGLE = l.findVarHandle(MemoryModelProbe.class, "single", float.class);
            DECIMAL = l.findVarHandle(MemoryModelProbe.class, "decimal", double.class);
            READY = l.findVarHandle(MemoryModelProbe.class, "ready", int.class);
        } catch (Exception e) { throw new ExceptionInInitializerError(e); }
    }
    static void check(boolean v) { checks++; if (!v) throw new AssertionError("check " + checks); }
    public static void main(String[] args) throws Exception {
        var p = new MemoryModelProbe();
        NUMBER.set(p, Integer.MAX_VALUE);
        check((int) NUMBER.getAndAdd(p, 1) == Integer.MAX_VALUE);
        check((int) NUMBER.get(p) == Integer.MIN_VALUE);
        WIDE.set(p, Long.MAX_VALUE);
        check((long) WIDE.getAndAdd(p, 1L) == Long.MAX_VALUE);
        check((long) WIDE.get(p) == Long.MIN_VALUE);
        SINGLE.set(p, -0.0f);
        check(!(boolean) SINGLE.compareAndSet(p, 0.0f, 1.0f));
        check((boolean) SINGLE.compareAndSet(p, -0.0f, 1.0f));
        float nan = Float.intBitsToFloat(0x7fc01234);
        SINGLE.set(p, nan);
        check(!(boolean) SINGLE.compareAndSet(p, Float.intBitsToFloat(0x7fc05678), 3.0f));
        check((boolean) SINGLE.compareAndSet(p, nan, 2.0f));
        DECIMAL.set(p, -0.0d);
        check(!(boolean) DECIMAL.compareAndSet(p, 0.0d, 1.0d));
        check((boolean) DECIMAL.compareAndSet(p, -0.0d, 1.0d));
        double dnan = Double.longBitsToDouble(0x7ff8000000001234L);
        DECIMAL.set(p, dnan);
        check((boolean) DECIMAL.compareAndSet(p, dnan, 2.0d));
        NUMBER.set(p, 0);
        Thread[] counters = new Thread[4];
        for (int k = 0; k < counters.length; k++) {
            counters[k] = new Thread(() -> { for (int n = 0; n < 500; n++) NUMBER.getAndAdd(p, 1); });
            counters[k].start();
        }
        for (Thread counter : counters) counter.join();
        check((int) NUMBER.getVolatile(p) == 2000);
        try { float ignored = (float) SINGLE.getAndBitwiseOr(p, 1.0f); throw new AssertionError(); }
        catch (UnsupportedOperationException expected) { }
        var readonly = MethodHandles.lookup().findVarHandle(MemoryModelProbe.class, "immutable", int.class);
        check((int) readonly.get(p) == 42);
        try { readonly.set(p, 2); throw new AssertionError(); }
        catch (UnsupportedOperationException expected) { }
        var bytes = MethodHandles.byteArrayViewVarHandle(int[].class, ByteOrder.LITTLE_ENDIAN);
        byte[] storage = new byte[12];
        bytes.set(storage, 0, Integer.MAX_VALUE);
        check((int) bytes.get(storage, 0) == Integer.MAX_VALUE);
        try { int ignored = (int) bytes.getAndAdd(storage, 0, 1); throw new AssertionError(); }
        catch (UnsupportedOperationException expected) { }
        try { int ignored = (int) bytes.getVolatile(storage, 0); throw new AssertionError(); }
        catch (UnsupportedOperationException expected) { }
        bytes.set(storage, 1, 9); // Plain unaligned access is supported.
        check((int) bytes.get(storage, 1) == 9);
        AtomicReference<Throwable> error = new AtomicReference<>();
        for (boolean release : new boolean[]{false, true}) {
            p.ready = 0;
            p.payload = 0;
            Thread writer = new Thread(() -> {
                for (int i = 1; i <= 1000; i++) {
                    while ((int) READY.getAcquire(p) != 0) Thread.onSpinWait();
                    p.payload = i;
                    if (release) READY.setRelease(p, 1); else p.ready = 1;
                }
            });
            Thread reader = new Thread(() -> {
                try {
                    for (int i = 1; i <= 1000; i++) {
                        while ((release ? (int) READY.getAcquire(p) : p.ready) == 0) Thread.onSpinWait();
                        check(p.payload == i);
                        READY.setRelease(p, 0);
                    }
                } catch (Throwable t) { error.set(t); }
            });
            writer.setDaemon(true); reader.setDaemon(true);
            writer.start(); reader.start(); writer.join(15000); reader.join(15000);
            if (error.get() != null) throw new AssertionError(error.get());
            check(!writer.isAlive() && !reader.isAlive());
        }
        System.out.println("Memory model: numeric atomics and publication passed");
    }
}
