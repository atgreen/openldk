// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.lang.management.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicReference;

public class CpuTimeProbe {
    static volatile long sink;
    static void check(boolean ok, String message) { if (!ok) throw new AssertionError(message); }
    public static void main(String[] args) throws Exception {
        ThreadMXBean bean = ManagementFactory.getThreadMXBean();
        if (!bean.isCurrentThreadCpuTimeSupported()) {
            try { bean.getCurrentThreadCpuTime(); throw new AssertionError("unsupported CPU time succeeded"); }
            catch (UnsupportedOperationException expected) { }
            System.out.println("Thread CPU accounting: capability and isolation passed");
            return;
        }
        boolean enabled = bean.isThreadCpuTimeEnabled();
        try {
            bean.setThreadCpuTimeEnabled(false);
            check(bean.getCurrentThreadCpuTime() == -1 && bean.getCurrentThreadUserTime() == -1, "disabled accounting");
            bean.setThreadCpuTimeEnabled(true);
            check(bean.getCurrentThreadCpuTime() >= 0, "missing current CPU time");
            // First run warms the blocking/JIT paths before measuring isolation.
            for (int trial = 0; trial < 2; trial++) {
            CountDownLatch entered = new CountDownLatch(1), finished = new CountDownLatch(1);
            AtomicReference<Throwable> failure = new AtomicReference<>();
            long[] workerTimes = new long[2];
            Thread worker = new Thread(() -> {
                try {
                    long before = bean.getCurrentThreadCpuTime();
                    entered.countDown();
                    long deadline = System.nanoTime() + 15_000_000_000L;
                    long sum = 1, elapsed;
                    do {
                        for (int i = 0; i < 10000; i++) sum = sum * 6364136223846793005L + i;
                        sink = sum;
                        elapsed = bean.getCurrentThreadCpuTime() - before;
                        check(System.nanoTime() < deadline, "CPU worker deadline");
                    } while (elapsed < 250_000_000L);
                    workerTimes[0] = elapsed;
                    workerTimes[1] = bean.getCurrentThreadUserTime();
                } catch (Throwable t) { failure.set(t); }
                finally { entered.countDown(); finished.countDown(); }
            });
            worker.setDaemon(true);
            worker.start();
            check(entered.await(10, TimeUnit.SECONDS), "worker did not start");
            long cpu = bean.getCurrentThreadCpuTime();
            long user = bean.getCurrentThreadUserTime();
            check(finished.await(20, TimeUnit.SECONDS), "worker did not finish");
            long mainCpu = bean.getCurrentThreadCpuTime() - cpu;
            long mainUser = bean.getCurrentThreadUserTime() - user;
            worker.join(5000);
            if (failure.get() != null) throw new AssertionError(failure.get());
            check(workerTimes[0] >= 250_000_000L, "worker CPU did not advance");
            check(mainCpu >= 0 && mainUser >= 0, "nonmonotonic CPU accounting");
            check(trial == 0 || mainCpu < workerTimes[0] / 2 + 20_000_000L,
                  "current-thread time includes worker CPU: main=" + mainCpu + " worker=" + workerTimes[0]);
            check(mainUser <= mainCpu + 10_000_000L && workerTimes[1] >= 0, "user time accounting");
            }
            AtomicReference<Throwable> failure = new AtomicReference<>();
            Thread virtual = Thread.ofVirtual().unstarted(() -> {
                check(bean.getCurrentThreadCpuTime() == -1, "virtual thread CPU time");
                check(bean.getCurrentThreadUserTime() == -1, "virtual thread user time");
            });
            virtual.setUncaughtExceptionHandler((t, e) -> failure.set(e));
            virtual.start(); virtual.join(5000);
            check(!virtual.isAlive(), "virtual worker did not finish");
            if (failure.get() != null) throw new AssertionError(failure.get());
        } finally { bean.setThreadCpuTimeEnabled(enabled); }
        System.out.println("Thread CPU accounting: capability and isolation passed");
    }
}
