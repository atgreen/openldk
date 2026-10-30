// Copyright (C) 2026 Anthony Green <green@moxielogic.com>
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

public class ThreadFailureProbe {
    static void check(boolean value, String message) {
        if (!value) throw new AssertionError(message);
    }
    static void join(Thread thread) throws Exception {
        thread.join(10000);
        check(!thread.isAlive(), "thread did not terminate");
    }
    public static void main(String[] args) throws Exception {
        Thread.UncaughtExceptionHandler previous = Thread.getDefaultUncaughtExceptionHandler();
        AtomicReference<Throwable> observed = new AtomicReference<>();
        AtomicReference<Thread> owner = new AtomicReference<>();
        RuntimeException failure = new RuntimeException("original");
        try {
            Thread.setDefaultUncaughtExceptionHandler((t, e) -> { owner.set(t); observed.set(e); });
            Thread fallback = new Thread(() -> { throw failure; });
            fallback.start(); join(fallback);
            check(observed.get() == failure && owner.get() == fallback, "default handler delivery");
            observed.set(null);
            Thread custom = new Thread(() -> { throw failure; });
            CountDownLatch entered = new CountDownLatch(1), release = new CountDownLatch(1);
            custom.setUncaughtExceptionHandler((t, e) -> {
                owner.set(t); observed.set(e); entered.countDown();
                try { release.await(); } catch (InterruptedException ex) { throw new AssertionError(ex); }
            });
            custom.start();
            try {
                check(entered.await(10, TimeUnit.SECONDS), "custom handler not entered");
                custom.join(25);
                check(custom.isAlive(), "termination published before handler finished");
                check(owner.get() == custom && observed.get() == failure, "custom handler delivery");
            } finally { release.countDown(); }
            join(custom);
            observed.set(null);
            ThreadGroup group = new ThreadGroup("failure-test") {
                public void uncaughtException(Thread t, Throwable e) { owner.set(t); observed.set(e); }
            };
            Thread grouped = new Thread(group, () -> { throw failure; });
            grouped.start(); join(grouped);
            check(owner.get() == grouped && observed.get() == failure, "thread group handler delivery");
            observed.set(null);
            Thread brokenHandler = new Thread(() -> { throw failure; });
            brokenHandler.setUncaughtExceptionHandler((t, e) -> { throw new IllegalStateException("handler"); });
            brokenHandler.start(); join(brokenHandler);
            check(observed.get() == null, "handler exception redispatched to default");
            Thread virtual = Thread.ofVirtual().unstarted(() -> { throw failure; });
            virtual.setUncaughtExceptionHandler((t, e) -> { owner.set(t); observed.set(e); });
            virtual.start(); join(virtual);
            check(owner.get() == virtual && observed.get() == failure, "virtual handler delivery");
        } finally { Thread.setDefaultUncaughtExceptionHandler(previous); }
        System.out.println("Thread uncaught exception delivery and handler lifetime passed");
    }
}
