// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.lang.invoke.*;
import java.nio.*;
import java.util.concurrent.atomic.AtomicReference;
import java.lang.foreign.Arena;

public class BufferViewProbe {
    static int checks;
    interface Action { void run() throws Throwable; }
    static void check(boolean ok) { checks++; if (!ok) throw new AssertionError("check " + checks); }
    static void fails(Class<? extends Throwable> type, Action action) throws Throwable {
        try { action.run(); } catch (Throwable t) {
            if (!type.isInstance(t)) throw new AssertionError("expected " + type + " at check " + checks, t);
            checks++; return;
        }
        throw new AssertionError("missing " + type + " at check " + checks);
    }
    static void plain(VarHandle h, ByteBuffer b, int index, int kind) {
        switch (kind) {
            case 0 -> { h.set(b, index, (short)-12345); check((short)h.get(b, index) == -12345); }
            case 1 -> { h.set(b, index, '\uffe1'); check((char)h.get(b, index) == '\uffe1'); }
            case 2 -> { h.set(b, index, 0x87654321); check((int)h.get(b, index) == 0x87654321); }
            case 3 -> { h.set(b, index, 0xfedcba9876543210L); check((long)h.get(b, index) == 0xfedcba9876543210L); }
            case 4 -> { h.set(b, index, -0.0f); check(Float.floatToRawIntBits((float)h.get(b, index)) == 0x80000000); }
            case 5 -> { h.set(b, index, -0.0d); check(Double.doubleToRawLongBits((double)h.get(b, index)) == Long.MIN_VALUE); }
        }
    }
    static void ordered(VarHandle h, ByteBuffer b, int kind) {
        switch (kind) {
            case 0 -> { h.setRelease(b, 0, (short)17); check((short)h.getAcquire(b, 0) == 17);
                        h.setVolatile(b, 0, (short)-2); check((short)h.getVolatile(b, 0) == -2);
                        h.setOpaque(b, 0, (short)9); check((short)h.getOpaque(b, 0) == 9); }
            case 1 -> { h.setRelease(b, 0, 'Z'); check((char)h.getAcquire(b, 0) == 'Z');
                        h.setVolatile(b, 0, 'Q'); check((char)h.getVolatile(b, 0) == 'Q');
                        h.setOpaque(b, 0, 'A'); check((char)h.getOpaque(b, 0) == 'A'); }
            case 2 -> { h.setRelease(b, 0, 17); check((int)h.getAcquire(b, 0) == 17);
                        h.setVolatile(b, 0, -2); check((int)h.getVolatile(b, 0) == -2);
                        h.setOpaque(b, 0, 9); check((int)h.getOpaque(b, 0) == 9); }
            case 3 -> { h.setRelease(b, 0, 17L); check((long)h.getAcquire(b, 0) == 17);
                        h.setVolatile(b, 0, -2L); check((long)h.getVolatile(b, 0) == -2);
                        h.setOpaque(b, 0, 9L); check((long)h.getOpaque(b, 0) == 9); }
            case 4 -> { h.setRelease(b, 0, 17f); check((float)h.getAcquire(b, 0) == 17);
                        h.setVolatile(b, 0, -2f); check((float)h.getVolatile(b, 0) == -2);
                        h.setOpaque(b, 0, 9f); check((float)h.getOpaque(b, 0) == 9); }
            case 5 -> { h.setRelease(b, 0, 17d); check((double)h.getAcquire(b, 0) == 17);
                        h.setVolatile(b, 0, -2d); check((double)h.getVolatile(b, 0) == -2);
                        h.setOpaque(b, 0, 9d); check((double)h.getOpaque(b, 0) == 9); }
        }
    }
    static void integers(VarHandle h, ByteBuffer b) {
        h.set(b, 0, 7);
        check((boolean)h.compareAndSet(b, 0, 7, 8));
        check(!(boolean)h.compareAndSet(b, 0, 7, 9));
        check((int)h.compareAndExchange(b, 0, 8, 10) == 8);
        check((int)h.compareAndExchangeAcquire(b, 0, 8, 11) == 10);
        check((int)h.compareAndExchangeRelease(b, 0, 10, 12) == 10);
        check((int)h.getAndSet(b, 0, 13) == 12);
        check((int)h.getAndSetAcquire(b, 0, 14) == 13);
        check((int)h.getAndSetRelease(b, 0, Integer.MAX_VALUE) == 14);
        check((int)h.getAndAdd(b, 0, 1) == Integer.MAX_VALUE);
        check((int)h.get(b, 0) == Integer.MIN_VALUE);
        check((int)h.getAndAddAcquire(b, 0, 1) == Integer.MIN_VALUE);
        check((int)h.getAndAddRelease(b, 0, 1) == Integer.MIN_VALUE + 1);
        h.set(b, 0, 0x30);
        check((int)h.getAndBitwiseOr(b, 0, 3) == 0x30);
        check((int)h.getAndBitwiseAndAcquire(b, 0, 0x0f) == 0x33);
        check((int)h.getAndBitwiseXorRelease(b, 0, 7) == 3);
        check((int)h.get(b, 0) == 4);
        while (!(boolean)h.weakCompareAndSetPlain(b, 0, 4, 5)) Thread.onSpinWait();
        while (!(boolean)h.weakCompareAndSetAcquire(b, 0, 5, 6)) Thread.onSpinWait();
        while (!(boolean)h.weakCompareAndSetRelease(b, 0, 6, 7)) Thread.onSpinWait();
        while (!(boolean)h.weakCompareAndSet(b, 0, 7, 8)) Thread.onSpinWait();
        check((int)h.get(b, 0) == 8);
    }
    static void atomics(ByteOrder order) throws Throwable {
        ByteBuffer b = ByteBuffer.allocateDirect(32);
        VarHandle h = MethodHandles.byteBufferViewVarHandle(int[].class, order);
        integers(h, b);
        VarHandle wide = MethodHandles.byteBufferViewVarHandle(long[].class, order);
        wide.set(b, 0, Long.MAX_VALUE);
        check((long)wide.getAndAdd(b, 0, 1L) == Long.MAX_VALUE);
        check((long)wide.getVolatile(b, 0) == Long.MIN_VALUE);
        check((boolean)wide.compareAndSet(b, 0, Long.MIN_VALUE, -1L));
        check((long)wide.getAndBitwiseAnd(b, 0, 0x123456789L) == -1L);
        check((long)wide.get(b, 0) == 0x123456789L);
        VarHandle floats = MethodHandles.byteBufferViewVarHandle(float[].class, order);
        floats.set(b, 0, -0f);
        check(!(boolean)floats.compareAndSet(b, 0, 0f, 1f));
        check((boolean)floats.compareAndSet(b, 0, -0f, 1f));
        float nan = Float.intBitsToFloat(0x7fc01234);
        floats.set(b, 0, nan);
        check(!(boolean)floats.compareAndSet(b, 0, Float.intBitsToFloat(0x7fc05678), 1f));
        check((boolean)floats.compareAndSet(b, 0, nan, 2f));
        check((float)floats.getAndSetAcquire(b, 0, 3f) == 2f);
        fails(UnsupportedOperationException.class, () -> { float v = (float)floats.getAndAdd(b, 0, 1f); });
        VarHandle doubles = MethodHandles.byteBufferViewVarHandle(double[].class, order);
        doubles.set(b, 0, -0d);
        check(!(boolean)doubles.compareAndSet(b, 0, 0d, 1d));
        check((double)doubles.compareAndExchangeRelease(b, 0, -0d, 2d) == -0d);
        fails(UnsupportedOperationException.class, () -> { double v = (double)doubles.getAndBitwiseOr(b, 0, 1d); });
        VarHandle shorts = MethodHandles.byteBufferViewVarHandle(short[].class, order);
        fails(UnsupportedOperationException.class, () -> shorts.compareAndSet(b, 0, (short)0, (short)1));
        // Shared backing storage through a differently ordered view.
        h.set(b, 0, 0x01020304);
        VarHandle opposite = MethodHandles.byteBufferViewVarHandle(int[].class,
            order == ByteOrder.BIG_ENDIAN ? ByteOrder.LITTLE_ENDIAN : ByteOrder.BIG_ENDIAN);
        check((int)opposite.get(b, 0) == 0x04030201);
        MethodHandle get = h.toMethodHandle(VarHandle.AccessMode.GET);
        MethodHandle set = h.toMethodHandle(VarHandle.AccessMode.SET);
        set.invokeExact(b, 0, 99);
        check((int)get.invokeExact(b, 0) == 99);
        check((long)h.get(b, 0) == 99L);
        check(((Integer)h.get(b, 0)).intValue() == 99);
        check((int)h.get((Object)b, 0) == 99);
        h.set(b, 0, (short)12);
        check((int)h.get(b, 0) == 12);
        fails(WrongMethodTypeException.class, () -> h.set(b, 0, 12L));
        fails(WrongMethodTypeException.class, () -> { boolean v = (boolean)h.get(b, 0); });
        fails(ClassCastException.class, () -> { int v = (int)h.get((Object)"bad", 0); });
        h.set(b, 0, 99);
        VarHandle exact = h.withInvokeExactBehavior();
        check((int)exact.get(b, 0) == 99);
        fails(WrongMethodTypeException.class, () -> { long v = (long)exact.get(b, 0); });
        h.set(b, 0, 0);
        AtomicReference<Throwable> failure = new AtomicReference<>();
        Thread[] workers = new Thread[4];
        for (int i = 0; i < workers.length; i++) {
            workers[i] = new Thread(() -> { try { for (int n = 0; n < 1000; n++) h.getAndAdd(b, 0, 1); }
                catch (Throwable t) { failure.set(t); } });
            workers[i].setDaemon(true); workers[i].start();
        }
        for (Thread t : workers) { t.join(15000); check(!t.isAlive()); }
        if (failure.get() != null) throw new AssertionError(failure.get());
        check((int)h.getVolatile(b, 0) == 4000);
    }
    public static void main(String[] args) throws Throwable {
        Class<?>[] kinds = {short[].class, char[].class, int[].class, long[].class, float[].class, double[].class};
        for (ByteOrder order : new ByteOrder[]{ByteOrder.BIG_ENDIAN, ByteOrder.LITTLE_ENDIAN}) {
            for (boolean direct : new boolean[]{false, true}) {
                for (int kind = 0; kind < kinds.length; kind++) {
                    VarHandle h = MethodHandles.byteBufferViewVarHandle(kinds[kind], order);
                    for (VarHandle.AccessMode mode : VarHandle.AccessMode.values()) {
                        String name = mode.methodName();
                        boolean orderedReadWrite = name.startsWith("get") && !name.startsWith("getAnd") || name.startsWith("set");
                        boolean supported = orderedReadWrite || (kind >= 2 &&
                            (kind < 4 || !name.startsWith("getAndAdd") && !name.startsWith("getAndBitwise")));
                        check(h.isAccessModeSupported(mode) == supported);
                    }
                    ByteBuffer b = direct ? ByteBuffer.allocateDirect(64) : ByteBuffer.allocate(64);
                    b.order(order == ByteOrder.BIG_ENDIAN ? ByteOrder.LITTLE_ENDIAN : ByteOrder.BIG_ENDIAN);
                    int position = 11;
                    b.position(position); b.limit(48);
                    plain(h, b, 0, kind); plain(h, b, 1, kind);
                    check(b.position() == position && b.limit() == 48);
                    ByteBuffer duplicate = b.duplicate();
                    plain(h, duplicate, 3, kind);
                    ByteBuffer slice = b.slice();
                    plain(h, slice, 0, kind);
                    ByteBuffer ro = b.asReadOnlyBuffer();
                    final int k = kind;
                    fails(ReadOnlyBufferException.class, () -> plain(h, ro, 0, k));
                    fails(IndexOutOfBoundsException.class, () -> plain(h, b, -1, k));
                    fails(IndexOutOfBoundsException.class, () -> plain(h, b, 47, k));
                    if (direct) ordered(h, b, kind);
                    else fails(IllegalStateException.class, () -> ordered(h, b, k));
                }
                VarHandle ints = MethodHandles.byteBufferViewVarHandle(int[].class, order);
                ByteBuffer b = direct ? ByteBuffer.allocateDirect(32) : ByteBuffer.allocate(32);
                ByteBuffer ro = b.asReadOnlyBuffer();
                fails(IllegalStateException.class, () -> { int v = (int)ints.getVolatile(b, 1); });
                fails(direct ? ReadOnlyBufferException.class : IllegalStateException.class,
                      () -> ints.setVolatile(ro, 1, 9));
                fails(ReadOnlyBufferException.class, () -> ints.set(ro, -1, 9));
                fails(NullPointerException.class, () -> { int v = (int)ints.get((ByteBuffer)null, 0); });
                if (direct) {
                    b.position(1);
                    ByteBuffer slice = b.slice();
                    fails(IllegalStateException.class, () -> { int v = (int)ints.getAcquire(slice, 0); });
                    ints.setVolatile(slice, 3, 77);
                    check((int)ints.getVolatile(b, 4) == 77);
                    check((int)ints.getAcquire(ro, 4) == 77);
                }
            }
            atomics(order);
        }
        VarHandle ints = MethodHandles.byteBufferViewVarHandle(int[].class, ByteOrder.nativeOrder());
        ByteBuffer closed;
        try (Arena arena = Arena.ofConfined()) {
            closed = arena.allocate(16, 8).asByteBuffer();
            ints.setVolatile(closed, 0, 42);
            check((int)ints.getVolatile(closed, 0) == 42);
            AtomicReference<Throwable> failure = new AtomicReference<>();
            Thread foreign = new Thread(() -> {
                try { int v = (int)ints.get(closed, 0); failure.set(new AssertionError("confined buffer readable by another thread")); }
                catch (WrongThreadException expected) { }
                catch (Throwable t) { failure.set(t); }
            });
            foreign.setDaemon(true); foreign.start(); foreign.join(5000);
            check(!foreign.isAlive());
            if (failure.get() != null) throw failure.get();
        }
        fails(IllegalStateException.class, () -> { int v = (int)ints.get(closed, 0); });
        System.out.println("Buffer views: heap/direct, endian, alignment, read-only and atomics passed");
    }
}
