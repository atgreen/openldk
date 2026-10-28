// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import module java.base;
import module java.sql;

public class ConstructionProbe {
    static final List<String> EVENTS = new ArrayList<>();
    static class Parent {
        final int value;
        Parent(int value) { this.value = value; EVENTS.add("parent:" + observed()); }
        int observed() { return -1; }
    }
    static class Child extends Parent {
        final int early;
        int late = 9;
        Child(int value) {
            if (value < 0) throw new IllegalArgumentException("negative");
            int doubled = Math.multiplyExact(value, 2);
            early = doubled;
            EVENTS.add("prologue:" + doubled);
            super(doubled + 1);
            EVENTS.add("epilogue:" + early + ":" + late);
        }
        int observed() { return early; }
        Child(String value) {
            int parsed = Integer.parseInt(value);
            EVENTS.add("delegate:" + parsed);
            this(parsed);
        }
    }
    record Pair(int left, int right) {}
    static void check(boolean value) { if (!value) throw new AssertionError(EVENTS); }
    public static void main(String[] args) {
        var child = new Child("21");
        check(child.value == 43 && child.early == 42 && child.late == 9);
        check(EVENTS.equals(List.of("delegate:21", "prologue:42", "parent:42", "epilogue:42:9")));
        EVENTS.clear();
        try { new Child(-1); throw new AssertionError(); } catch (IllegalArgumentException expected) { }
        check(EVENTS.isEmpty());
        // java.sql transitively requires java.logging: this type is imported too.
        Logger logger = Logger.getLogger("construction");
        check(logger != null && DriverManager.getDrivers() != null);
        check(new Pair(1, 2).left() == 1 && Stream.of(1, 2, 3).mapToInt(x -> x).sum() == 6);
        System.out.println("Java 25 language: module imports, constructor prologues, early fields, delegation, and failures passed");
    }
}
