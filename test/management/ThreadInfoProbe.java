// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.lang.management.*;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.locks.LockSupport;

public class ThreadInfoProbe {
    static final ThreadMXBean bean = ManagementFactory.getThreadMXBean();
    static final Object lock = new Object();
    static volatile boolean release;
    static volatile Throwable failure;
    static void awaitSignal(CountDownLatch ready) {
        synchronized (lock) {
            ready.countDown();
            try { while (!release) lock.wait(); }
            catch (Throwable error) { failure = error; }
        }
    }
    static void parkAtMarker(CountDownLatch ready) {
        ready.countDown();
        while (!release) LockSupport.park();
    }
    static void join(Thread thread) throws Exception {
        thread.join(5000);
        if (thread.isAlive()) throw new AssertionError("Thread did not finish: " + thread.getName());
        if (failure != null) throw new AssertionError("Sampled thread failed", failure);
    }
    static ThreadInfo state(Thread thread, Thread.State expected) throws Exception {
        long deadline = System.nanoTime() + 10_000_000_000L;
        ThreadInfo info;
        do {
            info = bean.getThreadInfo(thread.threadId(), Integer.MAX_VALUE);
            if (info != null && info.getThreadState() == expected) return info;
            Thread.sleep(5);
        } while (System.nanoTime() < deadline);
        throw new AssertionError("Expected " + expected + " for " + thread.getName() + ": " + info);
    }
    static void frame(ThreadInfo info, String method) {
        for (StackTraceElement element : info.getStackTrace())
            if (element.getClassName().equals(ThreadInfoProbe.class.getName())
                    && element.getMethodName().equals(method)) return;
        throw new AssertionError("Missing sampled method " + method + ": " + info);
    }
    static Thread worker(String name, Runnable task) {
        Thread thread = new Thread(task, name);
        thread.setDaemon(true);
        thread.start();
        return thread;
    }
    public static void main(String[] args) throws Exception {
        Thread main = Thread.currentThread();
        CountDownLatch ready = new CountDownLatch(1);
        Thread waiter = worker("managed-waiter", () -> awaitSignal(ready));
        ready.await();
        ThreadInfo waiting = state(waiter, Thread.State.WAITING);
        frame(waiting, "awaitSignal");
        if (!waiting.getThreadName().equals(waiter.getName()) || !waiting.isDaemon()
                || waiting.getPriority() != waiter.getPriority()) throw new AssertionError("Thread metadata");
        if (waiting.getLockInfo() == null || waiting.getLockInfo().getIdentityHashCode() != System.identityHashCode(lock)
                || waiting.getLockOwnerId() != -1) throw new AssertionError("Wait lock metadata");
        long[] ids = {main.threadId(), waiter.threadId(), Long.MAX_VALUE, main.threadId()};
        ThreadInfo[] infos = bean.getThreadInfo(ids, 2);
        if (infos.length != ids.length || infos[2] != null) throw new AssertionError("Missing thread/order");
        for (int i : new int[]{0, 1, 3})
            if (infos[i] == null || infos[i].getThreadId() != ids[i] || infos[i].getStackTrace().length > 2)
                throw new AssertionError("ID, duplicate, or depth");
        frame(bean.getThreadInfo(main.threadId(), Integer.MAX_VALUE), "main");
        ThreadInfo shallow = bean.getThreadInfo(waiter.threadId());
        if (shallow.getStackTrace().length != 0 || shallow.getLockedMonitors().length != 0
                || shallow.getLockedSynchronizers().length != 0) throw new AssertionError("Depth zero");
        if (bean.getThreadInfo(new long[0], 0).length != 0) throw new AssertionError("Empty IDs");
        Thread neverStarted = new Thread(() -> {});
        if (bean.getThreadInfo(neverStarted.threadId()) != null) throw new AssertionError("Unstarted thread");
        synchronized (lock) { release = true; lock.notifyAll(); }
        join(waiter);
        if (bean.getThreadInfo(waiter.threadId(), 5) != null) throw new AssertionError("Terminated thread");

        CountDownLatch sleeping = new CountDownLatch(1);
        Thread sleeper = worker("managed-sleeper", () -> {
            sleeping.countDown();
            try { Thread.sleep(30000); failure = new AssertionError("Sleep was not interrupted"); }
            catch (InterruptedException expected) { if (Thread.currentThread().isInterrupted()) failure = new AssertionError("Interrupt status"); }
        });
        sleeping.await();
        state(sleeper, Thread.State.TIMED_WAITING);
        if (sleeper.isInterrupted()) throw new AssertionError("Sampling interrupted sleeper");
        sleeper.interrupt();
        join(sleeper);

        CountDownLatch entering = new CountDownLatch(1);
        Thread blocked;
        synchronized (lock) {
            blocked = worker("managed-blocked", () -> {
                entering.countDown();
                synchronized (lock) { release = false; }
            });
            entering.await();
            ThreadInfo info = state(blocked, Thread.State.BLOCKED);
            if (info.getLockOwnerId() != main.threadId() || !info.getLockOwnerName().equals(main.getName())
                    || info.getLockInfo().getIdentityHashCode() != System.identityHashCode(lock))
                throw new AssertionError("Blocked lock owner");
        }
        join(blocked);
        CountDownLatch parked = new CountDownLatch(1);
        Thread parker = worker("managed-parker", () -> parkAtMarker(parked));
        parked.await();
        frame(state(parker, Thread.State.WAITING), "parkAtMarker");
        ThreadInfo[] dump = bean.dumpAllThreads(false, false, 3);
        boolean found = false;
        for (ThreadInfo info : dump) {
            if (info.getStackTrace().length > 3) throw new AssertionError("Dump max depth");
            if (info.getThreadId() == parker.threadId()) found = true;
        }
        if (!found) throw new AssertionError("All-thread dump missed worker");
        if (bean.getThreadInfo(new long[]{parker.threadId(), Long.MAX_VALUE}, false, false, 1)[1] != null)
            throw new AssertionError("Selected dump missing ID");
        release = true;
        LockSupport.unpark(parker);
        join(parker);
        try { bean.getThreadInfo(0, 0); throw new AssertionError("Invalid ID"); }
        catch (IllegalArgumentException expected) { }
        try { bean.getThreadInfo(main.threadId(), -1); throw new AssertionError("Invalid depth"); }
        catch (IllegalArgumentException expected) { }
        if (ManagementFactory.getRuntimeMXBean().getUptime() < 0) throw new AssertionError("Uptime");
        System.out.println("Thread management snapshots, stacks, states, locks, depth limits, and dumps passed");
    }
}
