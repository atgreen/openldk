// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.time.Duration;
import java.util.concurrent.*;
import java.util.concurrent.atomic.*;

public class StructuredConcurrencyProbe {
    static final ScopedValue<String> KEY = ScopedValue.newInstance();
    static void check(boolean condition) { if (!condition) throw new AssertionError(); }
    static void success() throws Exception {
        try (var scope = StructuredTaskScope.<Integer>open()) {
            var a = scope.fork(() -> 20);
            var b = scope.fork(() -> 22);
            try { a.get(); throw new AssertionError(); } catch (IllegalStateException expected) { }
            scope.join();
            check(a.get() + b.get() == 42);
        }
        try (var scope = StructuredTaskScope.open(StructuredTaskScope.Joiner.<Integer>allSuccessfulOrThrow())) {
            scope.fork(() -> 20); scope.fork(() -> 22);
            check(scope.join().mapToInt(StructuredTaskScope.Subtask::get).sum() == 42);
        }
        try (var scope = StructuredTaskScope.open(StructuredTaskScope.Joiner.<Integer>awaitAllSuccessfulOrThrow(),
                config -> config.withThreadFactory(Thread.ofPlatform().factory()))) {
            var task = scope.fork(() -> { check(!Thread.currentThread().isVirtual()); return 42; });
            scope.join(); check(task.get() == 42);
        }
    }
    static void cancellation() throws Exception {
        AtomicBoolean stopped = new AtomicBoolean();
        CountDownLatch started = new CountDownLatch(1);
        IllegalArgumentException boom = new IllegalArgumentException("task-boom");
        try (var scope = StructuredTaskScope.open()) {
            scope.fork(() -> { started.countDown(); try { Thread.sleep(60_000); } finally { stopped.set(true); } return null; });
            scope.fork(() -> { started.await(); throw boom; });
            try { scope.join(); throw new AssertionError(); }
            catch (StructuredTaskScope.FailedException expected) { check(expected.getCause() == boom); }
        }
        check(stopped.get());
    }
    static void timeout() throws Exception {
        AtomicBoolean stopped = new AtomicBoolean();
        CountDownLatch started = new CountDownLatch(1);
        try (var scope = StructuredTaskScope.open(StructuredTaskScope.Joiner.awaitAllSuccessfulOrThrow(),
                config -> config.withTimeout(Duration.ofSeconds(5)))) {
            scope.fork(() -> { started.countDown(); try { Thread.sleep(60_000); } finally { stopped.set(true); } return null; });
            check(started.await(20, TimeUnit.SECONDS));
            try { scope.join(); throw new AssertionError(); }
            catch (StructuredTaskScope.TimeoutException expected) { }
        }
        check(stopped.get());
    }
    static void ownership() throws Exception {
        AtomicReference<Throwable> failure = new AtomicReference<>();
        try (var scope = StructuredTaskScope.open()) {
            Thread worker = Thread.ofPlatform().start(() -> {
                try { scope.fork(() -> 1); failure.set(new AssertionError("non-owner fork")); }
                catch (WrongThreadException expected) { }
                catch (Throwable error) { failure.set(error); }
            });
            worker.join();
            scope.join();
        }
        if (failure.get() != null) throw new AssertionError(failure.get());
    }
    static void inheritance() throws Exception {
        ScopedValue.where(KEY, "parent").call(() -> {
            try (var scope = StructuredTaskScope.<String>open()) {
                var child = scope.fork(() -> {
                    check(KEY.get().equals("parent"));
                    return ScopedValue.where(KEY, "nested").call(() -> {
                        try (var nested = StructuredTaskScope.<String>open()) {
                            var grandchild = nested.fork(KEY::get);
                            nested.join(); return grandchild.get();
                        }
                    });
                });
                scope.join(); check(child.get().equals("nested") && KEY.get().equals("parent"));
            }
            return null;
        });
        check(!KEY.isBound());
    }
    static void interruption() throws Exception {
        CountDownLatch started = new CountDownLatch(1);
        AtomicBoolean stopped = new AtomicBoolean();
        AtomicReference<Throwable> failure = new AtomicReference<>();
        Thread owner = Thread.ofPlatform().start(() -> {
            try (var scope = StructuredTaskScope.open()) {
                scope.fork(() -> { started.countDown(); try { Thread.sleep(60_000); } finally { stopped.set(true); } return null; });
                try { scope.join(); throw new AssertionError("interrupted join returned"); }
                catch (InterruptedException expected) { }
            } catch (Throwable error) { failure.set(error); }
        });
        check(started.await(20, TimeUnit.SECONDS));
        owner.interrupt(); owner.join(20_000);
        check(!owner.isAlive() && stopped.get());
        if (failure.get() != null) throw new AssertionError(failure.get());
    }
    static void structure() throws Exception {
        try (var scope = StructuredTaskScope.open()) {
            ScopedValue.where(KEY, "changed").run(() -> {
                try { scope.fork(KEY::get); throw new AssertionError("changed bindings accepted"); }
                catch (StructureViolationException expected) { }
            });
            scope.join();
            try { scope.fork(() -> 1); throw new AssertionError("fork after join"); }
            catch (IllegalStateException expected) { }
        }
        try {
            ScopedValue.where(KEY, "leaked").run(() -> {
                StructuredTaskScope.open();
            });
            throw new AssertionError("unclosed scope accepted");
        } catch (StructureViolationException expected) { }
        check(!KEY.isBound());
    }
    public static void main(String[] args) throws Exception {
        System.err.println("structured: success"); success();
        System.err.println("structured: cancellation"); cancellation();
        System.err.println("structured: timeout"); timeout();
        System.err.println("structured: ownership"); ownership();
        System.err.println("structured: inheritance"); inheritance();
        System.err.println("structured: interruption"); interruption();
        System.err.println("structured: structure"); structure();
        System.out.println("Structured concurrency: results, cancellation, timeout, ownership, inheritance, and cleanup passed");
    }
}
