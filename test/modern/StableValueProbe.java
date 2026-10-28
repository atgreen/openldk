// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.*;
import java.util.function.*;

public class StableValueProbe {
    static void check(boolean value) { if (!value) throw new AssertionError(); }
    static void equal(Object actual, Object expected) {
        if (!Objects.equals(actual, expected)) throw new AssertionError(actual + " != " + expected);
    }
    static void race(boolean virtual, IntConsumer action) throws Exception {
        var start = new CountDownLatch(1);
        var failure = new AtomicReference<Throwable>();
        Thread[] workers = new Thread[12];
        for (int i = 0; i < workers.length; i++) {
            final int id = i;
            Runnable task = () -> {
                try { start.await(); action.accept(id); }
                catch (Throwable e) { failure.compareAndSet(null, e); }
            };
            workers[i] = virtual ? Thread.ofVirtual().unstarted(task) : Thread.ofPlatform().unstarted(task);
            workers[i].start();
        }
        start.countDown();
        for (Thread worker : workers) { worker.join(15000); check(!worker.isAlive()); }
        if (failure.get() != null) throw new AssertionError(failure.get());
    }
    public static void main(String[] args) throws Exception {
        var value = StableValue.<String>of();
        check(!value.isSet()); equal(value.orElse("fallback"), "fallback");
        try { value.orElseThrow(); throw new AssertionError(); } catch (NoSuchElementException expected) { }
        check(value.trySet(null)); check(value.isSet()); check(value.orElseThrow() == null);
        check(value.orElse("fallback") == null && !value.trySet("changed"));
        check(value.orElseSet(() -> { throw new AssertionError("supplier invoked after set"); }) == null);
        try { value.setOrThrow("changed"); throw new AssertionError(); } catch (IllegalStateException expected) { }
        var retry = StableValue.<String>of();
        var failure = new IllegalArgumentException("retry");
        try { retry.orElseSet(() -> { throw failure; }); throw new AssertionError(); }
        catch (IllegalArgumentException expected) { check(expected == failure); }
        check(!retry.isSet()); equal(retry.orElseSet(() -> "recovered"), "recovered");
        var recursive = StableValue.<String>of();
        try { recursive.orElseSet(() -> recursive.orElseSet(() -> "nested")); throw new AssertionError(); }
        catch (IllegalStateException expected) { }
        check(!recursive.isSet());
        try { recursive.orElseSet(() -> { recursive.trySet("nested"); return "outer"; }); throw new AssertionError(); }
        catch (IllegalStateException expected) { }
        check(!recursive.isSet()); recursive.setOrThrow("ok");
        for (boolean virtual : new boolean[]{false, true}) {
            for (int round = 0; round < 3; round++) {
                var shared = StableValue.<Object>of(); var calls = new AtomicInteger();
                Object token = new Object();
                race(virtual, id -> check(shared.orElseSet(() -> {
                    calls.incrementAndGet(); Thread.yield(); return token;
                }) == token));
                check(calls.get() == 1 && shared.orElseThrow() == token);
                var setters = StableValue.<Integer>of(); var winners = new AtomicInteger();
                race(virtual, id -> { if (setters.trySet(id)) winners.incrementAndGet(); });
                check(winners.get() == 1 && setters.isSet());
            }
            var calls = new AtomicInteger();
            Supplier<Object> supplier = StableValue.supplier(() -> { calls.incrementAndGet(); return new Object(); });
            var results = new Object[12]; race(virtual, id -> results[id] = supplier.get());
            check(calls.get() == 1);
            for (Object result : results) check(result == results[0]);
            var perIndex = new AtomicIntegerArray(4);
            var list = StableValue.list(4, index -> { perIndex.incrementAndGet(index); return "v" + index; });
            race(virtual, id -> equal(list.get(id % 4), "v" + (id % 4)));
            for (int i = 0; i < 4; i++) check(perIndex.get(i) == 1);
        }
        var indexedCalls = new AtomicIntegerArray(3);
        var indexed = StableValue.intFunction(3, index -> { indexedCalls.incrementAndGet(index); return index * 2; });
        race(false, id -> equal(indexed.apply(id % 3), (id % 3) * 2));
        for (int i = 0; i < 3; i++) check(indexedCalls.get(i) == 1);
        var mapCalls = new AtomicIntegerArray(3);
        var lazyMap = StableValue.map(Set.of(0, 1, 2), key -> { mapCalls.incrementAndGet(key); return "key" + key; });
        race(true, id -> equal(lazyMap.get(id % 3), "key" + (id % 3)));
        for (int i = 0; i < 3; i++) check(mapCalls.get(i) == 1);
        var attempts = new AtomicInteger();
        var recovered = StableValue.<String>of();
        race(false, id -> {
            try { equal(recovered.orElseSet(() -> {
                if (attempts.incrementAndGet() == 1) throw new IllegalArgumentException("first attempt");
                return "ok";
            }), "ok"); } catch (IllegalArgumentException expected) { equal(expected.getMessage(), "first attempt"); }
        });
        check(attempts.get() == 2); equal(recovered.orElseThrow(), "ok");
        var calls = new AtomicInteger();
        var function = StableValue.function(Set.of("a", "b"), key -> { calls.incrementAndGet(); return key.toUpperCase(); });
        equal(function.apply("a"), "A"); equal(function.apply("a"), "A"); check(calls.get() == 1);
        try { function.apply("unknown"); throw new AssertionError(); } catch (IllegalArgumentException expected) { }
        var map = StableValue.map(Set.of("x", "y"), key -> key.equals("x") ? null : "Y");
        check(map.containsKey("x") && map.get("x") == null); equal(map.get("y"), "Y");
        try { map.put("z", "Z"); throw new AssertionError(); } catch (UnsupportedOperationException expected) { }
        System.out.println("Stable values: nulls, retries, reentry, concurrent initialization, one-winner writes, and lazy collections passed");
    }
}
