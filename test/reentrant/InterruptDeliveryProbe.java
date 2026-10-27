// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicReference;

public class InterruptDeliveryProbe {
    static volatile boolean release;
    static void join(Thread thread) throws Exception {
        thread.join(5000);
        if (thread.isAlive()) throw new AssertionError("Thread did not finish: " + thread.getName());
    }
    public static void main(String[] args) throws Exception {
        AtomicReference<Throwable> failure = new AtomicReference<>();
        CountDownLatch running = new CountDownLatch(1);
        Thread ordinary = new Thread(() -> {
            try {
                running.countDown();
                while (!release) Thread.onSpinWait();
                if (!Thread.interrupted()) throw new AssertionError("Interrupt status lost");
            } catch (Throwable error) { failure.set(error); }
        });
        ordinary.setDaemon(true);
        ordinary.start();
        running.await();
        ordinary.interrupt();
        Thread.sleep(30);
        release = true;
        join(ordinary);
        if (failure.get() != null) throw new AssertionError("Interrupt escaped into ordinary code", failure.get());

        Object lock = new Object();
        Thread main = Thread.currentThread();
        Thread interrupter = new Thread(() -> {
            try { Thread.sleep(30); } catch (InterruptedException error) { throw new AssertionError(error); }
            main.interrupt();
        });
        synchronized (lock) {
            interrupter.start();
            try { lock.wait(5000); throw new AssertionError("Main wait was not interrupted"); }
            catch (InterruptedException expected) {
                if (Thread.currentThread().isInterrupted()) throw new AssertionError("Interrupt status not cleared");
                if (!Thread.holdsLock(lock)) throw new AssertionError("Wait did not reacquire monitor");
            }
        }
        join(interrupter);
        Thread sleepInterrupter = new Thread(() -> {
            try { Thread.sleep(30); } catch (InterruptedException error) { throw new AssertionError(error); }
            main.interrupt();
        });
        sleepInterrupter.start();
        long beforeSleep = System.nanoTime();
        try { Thread.sleep(10000); throw new AssertionError("Main sleep was not interrupted"); }
        catch (InterruptedException expected) {
            if (main.isInterrupted()) throw new AssertionError("Sleep did not clear interrupt status");
        }
        if (System.nanoTime() - beforeSleep > 8_000_000_000L)
            throw new AssertionError("Main sleep only ended at its original deadline");
        join(sleepInterrupter);

        for (int round = 0; round < 20; round++) {
            CountDownLatch ready = new CountDownLatch(10);
            Thread[] waiters = new Thread[10];
            int[] outcomes = new int[10];
            for (int i = 0; i < waiters.length; i++) {
                final int index = i;
                waiters[i] = new Thread(() -> {
                    try {
                        synchronized (lock) {
                            ready.countDown();
                            try { lock.wait(); outcomes[index] = 1; }
                            catch (InterruptedException expected) {
                                if (!Thread.holdsLock(lock) || Thread.currentThread().isInterrupted())
                                    throw new AssertionError("Interrupted wait ownership or status");
                                outcomes[index] = 2;
                            }
                        }
                    } catch (Throwable error) { failure.set(error); }
                });
                waiters[i].setDaemon(true);
                waiters[i].start();
            }
            ready.await();
            for (int i = 0; i < 3; i++) waiters[i].interrupt();
            for (int i = 0; i < 3; i++) join(waiters[i]);
            synchronized (lock) {
                if ((round & 1) == 0) for (int i = 3; i < 10; i++) lock.notify();
                else lock.notifyAll();
            }
            for (Thread waiter : waiters) join(waiter);
            if (failure.get() != null) throw new AssertionError("Waiter failed", failure.get());
            for (int i = 0; i < 10; i++)
                if (outcomes[i] != (i < 3 ? 2 : 1)) throw new AssertionError("Notification or interrupt lost");
        }
        System.out.println("Interrupt delivery, main-thread waits, monitor ownership, and notification accounting passed");
    }
}
