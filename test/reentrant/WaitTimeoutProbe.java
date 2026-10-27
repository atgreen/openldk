// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
public class WaitTimeoutProbe {
    public static void main(String[] args) throws Exception {
        Object lock = new Object();
        Thread notifier = new Thread(() -> {
            try { Thread.sleep(100); } catch (InterruptedException e) { throw new AssertionError(e); }
            synchronized (lock) { lock.notifyAll(); }
        });
        synchronized (lock) {
            notifier.start();
            lock.wait(Long.MAX_VALUE);
        }
        notifier.join();
        System.out.println("Long.MAX_VALUE monitor wait and notification passed");
    }
}
