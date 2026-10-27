/* { dg-output "OK" } */
package aaa;
public class arraymonitor {
    static final Object lock = new boolean[1];
    static volatile boolean ready, released, interrupted;
    static volatile Throwable failure;
    public static void main(String[] args) throws Exception {
        try { lock.notify(); throw new AssertionError(); }
        catch (IllegalMonitorStateException expected) { }
        synchronized (lock) {
            try { lock.wait(-1); throw new AssertionError(); }
            catch (IllegalArgumentException expected) { }
            try { lock.wait(0, 1000000); throw new AssertionError(); }
            catch (IllegalArgumentException expected) { }
            lock.wait(1, 1);
            Thread.currentThread().interrupt();
            try { lock.wait(); throw new AssertionError(); }
            catch (InterruptedException expected) {
                if (!Thread.holdsLock(lock) || Thread.currentThread().isInterrupted())
                    throw new AssertionError("Pre-interrupted wait");
            }
        }
        Thread waiter = new Thread(() -> {
            try {
                synchronized (lock) {
                    synchronized (lock) {
                        ready = true;
                        lock.notifyAll();
                        while (!released) lock.wait();
                        ready = false;
                        lock.notify();
                        try {
                            while (true) lock.wait(10000);
                        } catch (InterruptedException expected) {
                            if (!Thread.holdsLock(lock) || Thread.currentThread().isInterrupted())
                                throw new AssertionError("Interrupted wait lost monitor or status");
                            interrupted = true;
                            lock.notifyAll();
                        }
                    }
                    if (!Thread.holdsLock(lock)) throw new AssertionError("Recursion lost");
                }
            } catch (Throwable problem) { failure = problem; }
        });
        waiter.start();
        synchronized (lock) {
            while (!ready) lock.wait(10000);
            released = true;
            lock.notifyAll();
            while (ready) lock.wait(10000);
            waiter.interrupt();
            while (!interrupted && failure == null) lock.wait(10000);
        }
        waiter.join(10000);
        if (waiter.isAlive() || failure != null || !interrupted)
            throw new AssertionError("Array monitor failed", failure);
        System.out.println("OK");
    }
}
