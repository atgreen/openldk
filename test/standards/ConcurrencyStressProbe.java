// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.lang.invoke.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicReference;

public class ConcurrencyStressProbe {
    static final int ITERATIONS = 5000;
    int payload;
    volatile int ready;
    int counter;
    static final VarHandle READY, COUNTER;
    static final AtomicReference<Throwable> error = new AtomicReference<>();
    static {
        try {
            var lookup = MethodHandles.lookup();
            READY = lookup.findVarHandle(ConcurrencyStressProbe.class, "ready", int.class);
            COUNTER = lookup.findVarHandle(ConcurrencyStressProbe.class, "counter", int.class);
        } catch (Exception e) { throw new ExceptionInInitializerError(e); }
    }
    static void check(boolean ok, String message) { if (!ok) throw new AssertionError(message); }
    static Thread start(boolean virtual, Runnable action) {
        Runnable guarded = () -> { try { action.run(); } catch (Throwable t) { error.compareAndSet(null, t); } };
        Thread t = virtual ? Thread.ofVirtual().unstarted(guarded) : new Thread(guarded);
        if (!virtual) t.setDaemon(true);
        t.start(); return t;
    }
    static void join(Thread... threads) throws Exception {
        for (Thread t : threads) {
            t.join(30000);
            check(!t.isAlive(), "worker timed out");
        }
        if (error.get() != null) throw new AssertionError("worker failed", error.get());
    }
    static void release(int mode) {
        switch (mode) {
            case 2 -> VarHandle.releaseFence();
            case 3 -> VarHandle.fullFence();
            case 4 -> VarHandle.storeStoreFence();
        }
    }
    static void acquire(int mode) {
        switch (mode) {
            case 2 -> VarHandle.acquireFence();
            case 3 -> VarHandle.fullFence();
            case 4 -> VarHandle.loadLoadFence();
        }
    }
    static int read(ConcurrencyStressProbe p, int mode) {
        return mode == 0 ? p.ready : mode == 1 ? (int)READY.getAcquire(p) : (int)READY.getOpaque(p);
    }
    static void write(ConcurrencyStressProbe p, int mode, int value) {
        if (mode == 0) p.ready = value;
        else if (mode == 1) READY.setRelease(p, value);
        else READY.setOpaque(p, value);
    }
    static void publication(boolean virtual, int mode) throws Exception {
        var p = new ConcurrencyStressProbe();
        Thread producer = start(virtual, () -> {
            for (int i = 1; i <= ITERATIONS; i++) {
                while ((int)READY.getAcquire(p) != 0) Thread.onSpinWait();
                p.payload = i;
                release(mode);
                write(p, mode, 1);
            }
        });
        Thread consumer = start(virtual, () -> {
            for (int i = 1; i <= ITERATIONS; i++) {
                while (read(p, mode) != 1) Thread.onSpinWait();
                acquire(mode);
                check(p.payload == i, "stale publication mode " + mode + " iteration " + i);
                READY.setRelease(p, 0);
            }
        });
        join(producer, consumer);
    }
    static void cas(boolean virtual) throws Exception {
        var p = new ConcurrencyStressProbe();
        Thread[] workers = new Thread[4];
        for (int k = 0; k < workers.length; k++) {
            workers[k] = start(virtual, () -> {
                for (int i = 0; i < ITERATIONS; i++) {
                    int old;
                    do { old = (int)COUNTER.getVolatile(p); }
                    while (!(boolean)COUNTER.compareAndSet(p, old, old + 1));
                }
            });
        }
        join(workers);
        check((int)COUNTER.getVolatile(p) == workers.length * ITERATIONS, "lost CAS update");
    }
    static void monitors(boolean virtual) throws Exception {
        Object lock = new Object();
        var p = new ConcurrencyStressProbe();
        Thread writer = start(virtual, () -> {
            synchronized (lock) {
                try {
                    for (int i = 1; i <= ITERATIONS; i++) {
                        while (p.ready != 0) lock.wait();
                        p.payload = i; p.ready = 1; lock.notifyAll();
                    }
                } catch (InterruptedException e) { throw new AssertionError(e); }
            }
        });
        Thread reader = start(virtual, () -> {
            synchronized (lock) {
                try {
                    for (int i = 1; i <= ITERATIONS; i++) {
                        while (p.ready != 1) lock.wait();
                        check(p.payload == i, "monitor publication");
                        p.ready = 0; lock.notifyAll();
                    }
                } catch (InterruptedException e) { throw new AssertionError(e); }
            }
        });
        join(writer, reader);
    }
    static void interruption(boolean virtual) throws Exception {
        Object lock = new Object();
        CountDownLatch entered = new CountDownLatch(1), interrupted = new CountDownLatch(1);
        Thread waiter = start(virtual, () -> {
            synchronized (lock) {
                entered.countDown();
                try { lock.wait(); throw new AssertionError("wait returned without interrupt"); }
                catch (InterruptedException expected) {
                    check(!Thread.currentThread().isInterrupted(), "wait did not clear interrupt");
                    check(Thread.holdsLock(lock), "wait did not reacquire monitor");
                    interrupted.countDown();
                }
            }
        });
        check(entered.await(10, TimeUnit.SECONDS), "waiter did not enter");
        synchronized (lock) { waiter.interrupt(); }
        join(waiter);
        check(interrupted.getCount() == 0, "wait interruption missing");
        Thread preinterrupted = start(virtual, () -> {
            Thread.currentThread().interrupt();
            try { Thread.sleep(30000); throw new AssertionError("sleep ignored interrupt"); }
            catch (InterruptedException expected) { check(!Thread.interrupted(), "sleep interrupt status"); }
            Thread.currentThread().interrupt();
            check(Thread.interrupted(), "interrupt status missing");
            check(!Thread.interrupted(), "interrupt status not cleared");
        });
        join(preinterrupted);
    }
    static void cancellation(boolean virtual) throws Exception {
        CountDownLatch entered = new CountDownLatch(1), cleaned = new CountDownLatch(1);
        FutureTask<Integer> task = new FutureTask<>(() -> {
            try {
                entered.countDown();
                new CountDownLatch(1).await();
                throw new AssertionError("cancel did not interrupt");
            } catch (InterruptedException expected) {
                check(!Thread.currentThread().isInterrupted(), "cancel interrupt status");
                return 42;
            } finally { cleaned.countDown(); }
        });
        Thread worker = start(virtual, task);
        check(entered.await(10, TimeUnit.SECONDS), "cancel task did not enter");
        check(task.cancel(true), "cancel failed");
        try { task.get(); throw new AssertionError("cancelled get succeeded"); }
        catch (CancellationException expected) { }
        check(cleaned.await(10, TimeUnit.SECONDS), "cancel cleanup missing");
        join(worker);
        check(task.isDone() && task.isCancelled(), "cancel task state");
        check(!task.cancel(true), "cancel succeeded twice");
    }
    public static void main(String[] args) throws Exception {
        for (int round = 0; round < 3; round++) {
            for (boolean virtual : new boolean[]{false, true}) {
                for (int mode = 0; mode < 5; mode++) publication(virtual, mode);
                cas(virtual); monitors(virtual); interruption(virtual); cancellation(virtual);
            }
        }
        System.out.println("Concurrency stress: volatile, CAS, fences, monitors, interruption and cancellation passed");
    }
}
