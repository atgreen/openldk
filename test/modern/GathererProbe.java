// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.*;
import java.util.stream.*;

public class GathererProbe {
    static void check(boolean value) { if (!value) throw new AssertionError(); }
    static void equal(Object actual, Object expected) {
        if (!Objects.equals(actual, expected)) throw new AssertionError(actual + " != " + expected);
    }
    static Stream<Integer> input(boolean parallel) {
        var stream = IntStream.rangeClosed(1, 8).boxed();
        return parallel ? stream.parallel() : stream;
    }
    public static void main(String[] args) {
        for (boolean parallel : new boolean[]{false, true}) {
            var windows = input(parallel).gather(Gatherers.windowFixed(3)).toList();
            equal(windows, List.of(List.of(1, 2, 3), List.of(4, 5, 6), List.of(7, 8)));
            try { windows.getFirst().add(0); throw new AssertionError(); }
            catch (UnsupportedOperationException expected) { }
            equal(input(parallel).gather(Gatherers.windowSliding(3)).toList(),
                    List.of(List.of(1, 2, 3), List.of(2, 3, 4), List.of(3, 4, 5),
                            List.of(4, 5, 6), List.of(5, 6, 7), List.of(6, 7, 8)));
            equal(input(parallel).gather(Gatherers.scan(() -> 0, Integer::sum)).toList(),
                    List.of(1, 3, 6, 10, 15, 21, 28, 36));
            equal(input(parallel).gather(Gatherers.fold(() -> "", (s, i) -> s + i)).toList(), List.of("12345678"));
            var composed = Gatherers.<Integer>windowFixed(3).andThen(
                    Gatherer.<List<Integer>, Integer>of((state, window, downstream) ->
                            downstream.push(window.stream().mapToInt(Integer::intValue).sum())));
            equal(input(parallel).gather(composed).toList(), List.of(6, 15, 15));
            // Stateful gatherer: each partition accumulates, combiner merges, finisher emits.
            Gatherer<Integer, int[], Integer> sum = Gatherer.of(() -> new int[1],
                    Gatherer.Integrator.ofGreedy((state, item, downstream) -> { state[0] += item; return true; }),
                    (left, right) -> { left[0] += right[0]; return left; },
                    (state, downstream) -> downstream.push(state[0]));
            equal(input(parallel).gather(sum).toList(), List.of(36));
            equal(input(parallel).gather(Gatherers.mapConcurrent(3, x -> x * x)).toList(),
                    List.of(1, 4, 9, 16, 25, 36, 49, 64));
        }
        var combinations = new AtomicInteger();
        Gatherer<Integer, long[], Long> parallelSum = Gatherer.of(() -> new long[1],
                Gatherer.Integrator.ofGreedy((state, item, downstream) -> { state[0] += item; return true; }),
                (left, right) -> { combinations.incrementAndGet(); left[0] += right[0]; return left; },
                (state, downstream) -> downstream.push(state[0]));
        equal(IntStream.rangeClosed(1, 2048).boxed().parallel().gather(parallelSum).toList(), List.of(2098176L));
        check(combinations.get() > 0);
        equal(Stream.<Integer>empty().gather(Gatherers.windowFixed(3)).toList(), List.of());
        equal(Stream.of(1, 2).gather(Gatherers.windowSliding(3)).toList(), List.of(List.of(1, 2)));
        var seen = new AtomicInteger();
        equal(Stream.iterate(0, x -> x + 1).peek(x -> seen.incrementAndGet())
                .gather(Gatherers.windowFixed(3)).limit(2).toList(), List.of(List.of(0, 1, 2), List.of(3, 4, 5)));
        check(seen.get() == 6);
        Gatherer<Integer, Void, Integer> untilFive = Gatherer.ofSequential((state, item, downstream) ->
                item < 5 && downstream.push(item));
        equal(Stream.iterate(0, x -> x + 1).gather(untilFive).toList(), List.of(0, 1, 2, 3, 4));
        equal(Stream.iterate(0, x -> x + 1).gather(Gatherers.mapConcurrent(2, x -> x + 1))
                .limit(4).toList(), List.of(1, 2, 3, 4));
        var active = new AtomicInteger(); var peak = new AtomicInteger(); var count = new AtomicInteger();
        var firstBatch = new CountDownLatch(3);
        equal(IntStream.range(0, 12).boxed().gather(Gatherers.mapConcurrent(3, x -> {
            int n = active.incrementAndGet(); peak.accumulateAndGet(n, Math::max);
            try {
                if (x < 3) {
                    firstBatch.countDown();
                    check(firstBatch.await(15, TimeUnit.SECONDS));
                }
                Thread.sleep(5); count.incrementAndGet(); return x;
            }
            catch (InterruptedException e) { throw new RuntimeException(e); }
            finally { active.decrementAndGet(); }
        })).toList(), IntStream.range(0, 12).boxed().toList());
        check(active.get() == 0 && count.get() == 12 && peak.get() == 3);
        try {
            input(false).gather(Gatherers.mapConcurrent(2, x -> { if (x == 3) throw new IllegalArgumentException("mapper"); return x; })).toList();
            throw new AssertionError("mapper failure lost");
        } catch (IllegalArgumentException expected) { equal(expected.getMessage(), "mapper"); }
        for (Runnable invalid : List.<Runnable>of(() -> Gatherers.windowFixed(0),
                () -> Gatherers.windowSliding(-1), () -> Gatherers.mapConcurrent(0, x -> x))) {
            try { invalid.run(); throw new AssertionError(); } catch (IllegalArgumentException expected) { }
        }
        System.out.println("Gatherers: windows, scan/fold, composition, parallel reduction, short-circuiting, and concurrent mapping passed");
    }
}
