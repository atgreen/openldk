// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.util.*;

public class SequencedProbe {
    static void check(boolean value) { if (!value) throw new AssertionError(); }
    static void equal(Object actual, Object expected) {
        if (!Objects.equals(actual, expected)) throw new AssertionError(actual + " != " + expected);
    }
    static void unsupported(Runnable action) {
        try { action.run(); throw new AssertionError("mutation accepted"); }
        catch (UnsupportedOperationException expected) { }
    }
    static void empty(SequencedCollection<?> collection) {
        for (Runnable action : List.<Runnable>of(() -> collection.getFirst(), () -> collection.getLast(),
                () -> collection.removeFirst(), () -> collection.removeLast())) {
            try { action.run(); throw new AssertionError("empty endpoint accepted"); }
            catch (NoSuchElementException expected) { }
        }
    }
    public static void main(String[] args) {
        for (List<Integer> list : List.<List<Integer>>of(new ArrayList<>(), new LinkedList<>())) {
            list.addAll(List.of(1, 2, 3));
            List<Integer> reverse = list.reversed();
            reverse.addFirst(4); reverse.addLast(0);
            equal(list, List.of(0, 1, 2, 3, 4));
            equal(reverse.removeFirst(), 4); equal(reverse.removeLast(), 0);
            reverse.set(1, 20); equal(list, List.of(1, 20, 3));
            reverse.subList(0, 2).clear(); equal(list, List.of(1));
            list.addLast(2); equal(reverse, List.of(2, 1));
            equal(reverse.reversed(), list);
            var iterator = reverse.iterator(); equal(iterator.next(), 2); iterator.remove();
            equal(list, List.of(1)); list.clear(); empty(reverse);
        }
        var set = new LinkedHashSet<>(List.of("a", "b", "c"));
        var reversedSet = set.reversed();
        reversedSet.addFirst("a"); equal(new ArrayList<>(set), List.of("b", "c", "a"));
        reversedSet.addLast("c"); equal(new ArrayList<>(set), List.of("c", "b", "a"));
        equal(reversedSet.removeFirst(), "a"); set.addFirst("z");
        equal(new ArrayList<>(reversedSet), List.of("b", "c", "z"));
        var map = new LinkedHashMap<String, Integer>();
        map.put("a", 1); map.put("b", 2); map.put("c", 3);
        var reversed = map.reversed();
        var keys = reversed.sequencedKeySet(); var values = reversed.sequencedValues();
        var entries = reversed.sequencedEntrySet();
        reversed.putFirst("a", 10); reversed.putLast("d", 4);
        equal(new ArrayList<>(map.keySet()), List.of("d", "b", "c", "a"));
        equal(new ArrayList<>(keys), List.of("a", "c", "b", "d"));
        equal(new ArrayList<>(values), List.of(10, 3, 2, 4));
        unsupported(() -> entries.getFirst().setValue(99));
        entries.iterator().next().setValue(11); equal(map.get("a"), 11);
        var snapshot = reversed.firstEntry();
        unsupported(() -> snapshot.setValue(99));
        map.put("a", 12); equal(snapshot.getValue(), 11);
        equal(keys.removeLast(), "d"); equal(values.removeFirst(), 12);
        equal(entries.removeFirst(), Map.entry("c", 3)); equal(map, Map.of("b", 2));
        map.putFirst("x", 9); equal(keys.getLast(), "x");
        equal(reversed.pollFirstEntry(), Map.entry("b", 2));
        equal(reversed.pollLastEntry(), Map.entry("x", 9));
        check(reversed.firstEntry() == null && reversed.lastEntry() == null);
        check(reversed.pollFirstEntry() == null && reversed.pollLastEntry() == null);
        empty(keys);
        var readonly = Collections.unmodifiableSequencedMap(map);
        map.put("new", 7); equal(readonly.reversed().firstEntry(), Map.entry("new", 7));
        unsupported(() -> readonly.putFirst("bad", 0));
        unsupported(() -> readonly.reversed().pollFirstEntry());
        unsupported(() -> readonly.sequencedEntrySet().getFirst().setValue(0));
        unsupported(() -> readonly.sequencedValues().removeLast());
        var roSet = Collections.unmodifiableSequencedSet(set);
        unsupported(() -> roSet.reversed().addFirst("bad"));
        var roList = Collections.unmodifiableSequencedCollection(new ArrayList<>(List.of(1, 2)));
        unsupported(() -> roList.reversed().removeFirst());
        var access = new LinkedHashMap<String, Integer>(16, 0.75f, true);
        access.put("a", 1); access.put("b", 2); access.put("c", 3);
        equal(access.reversed().get("a"), 1);
        equal(new ArrayList<>(access.keySet()), List.of("b", "c", "a"));
        access.reversed().putLast("a", 10);
        equal(new ArrayList<>(access.keySet()), List.of("a", "b", "c"));
        var sortedMap = new TreeMap<Integer, String>();
        sortedMap.put(2, "b"); sortedMap.put(1, "a"); sortedMap.put(3, "c");
        equal(sortedMap.reversed().pollFirstEntry(), Map.entry(3, "c"));
        equal(new ArrayList<>(sortedMap.sequencedKeySet()), List.of(1, 2));
        unsupported(() -> sortedMap.putFirst(0, "z"));
        var sorted = new TreeSet<>(List.of(3, 1, 2));
        equal(new ArrayList<>(sorted.reversed()), List.of(3, 2, 1));
        unsupported(() -> sorted.addFirst(0)); unsupported(() -> sorted.reversed().addLast(4));
        System.out.println("Sequenced collections: backed/reversed views, endpoints, ordering, entries, and unmodifiable views passed");
    }
}
