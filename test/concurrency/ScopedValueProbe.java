// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.util.NoSuchElementException;
import java.util.concurrent.atomic.AtomicReference;

public class ScopedValueProbe {
    static final ScopedValue<String> KEY = ScopedValue.newInstance();
    static final ScopedValue<Integer> OTHER = ScopedValue.newInstance();
    static void check(boolean condition) { if (!condition) throw new AssertionError(); }
    static void absent() {
        check(!KEY.isBound() && KEY.orElse("fallback").equals("fallback"));
        try { KEY.get(); throw new AssertionError(); } catch (NoSuchElementException expected) { }
    }
    public static void main(String[] args) throws Exception {
        absent();
        ScopedValue.where(KEY, "outer").run(() -> {
            for (int i = 0; i < 100; i++) check(KEY.get().equals("outer"));
            ScopedValue.where(KEY, "inner").where(OTHER, 42).run(() -> {
                check(KEY.get().equals("inner") && OTHER.get() == 42);
            });
            check(KEY.get().equals("outer") && !OTHER.isBound());
            try { ScopedValue.where(KEY, "exception").run(() -> { throw new IllegalStateException("test"); }); }
            catch (IllegalStateException expected) { check(expected.getMessage().equals("test")); }
            check(KEY.get().equals("outer"));
        });
        absent();
        check(ScopedValue.where(KEY, "call").call(() -> KEY.get()).equals("call"));
        ScopedValue.where(KEY, null).run(() -> check(KEY.isBound() && KEY.get() == null));
        for (boolean virtual : new boolean[]{false, true}) {
            AtomicReference<Throwable> failure = new AtomicReference<>();
            ScopedValue.where(KEY, "parent").run(() -> {
                Thread[] workers = new Thread[8];
                for (int i = 0; i < workers.length; i++) {
                    final String value = "child-" + i;
                    Runnable task = () -> {
                        try {
                            absent();
                            ScopedValue.where(KEY, value).run(() -> {
                                for (int j = 0; j < 100; j++) { check(KEY.get().equals(value)); Thread.yield(); }
                            });
                            absent();
                        } catch (Throwable error) { failure.compareAndSet(null, error); }
                    };
                    workers[i] = virtual ? Thread.ofVirtual().unstarted(task) : Thread.ofPlatform().unstarted(task);
                    workers[i].start();
                }
                for (Thread worker : workers) {
                    try { worker.join(); } catch (InterruptedException e) { throw new AssertionError(e); }
                }
                check(KEY.get().equals("parent"));
            });
            if (failure.get() != null) throw new AssertionError(failure.get());
            absent();
        }
        System.out.println("Scoped values: nesting, cleanup, nulls, calls, and platform/virtual thread isolation passed");
    }
}
