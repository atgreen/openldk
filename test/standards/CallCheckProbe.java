// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
public class CallCheckProbe {
    static int events;
    static int initial = own(3);
    static int own(int x) { return x + 1; }
    static void check(boolean value) { if (!value) throw new AssertionError(); }
    static int argument(int x) { events = events * 10 + x; return x; }
    static class Cold {
        static { events = events * 10 + 9; }
        static int add(int a, int b) { return a + b; }
    }
    static int coldLoop(int n) {
        int s = 0;
        for (int i = 0; i < n; i++) s += Cold.add(argument(1), argument(2));
        return s;
    }
    static class Broken {
        static { events++; if (events > 0) throw new RuntimeException("broken"); }
        static int get() { return 1; }
    }
    static int brokenLoop(int n) {
        int s = 0;
        for (int i = 0; i < n; i++) s += Broken.get();
        return s;
    }
    static class Parent { static int value = 7; static int get() { return value; } }
    static class Child extends Parent {
        static int inherited() { return Child.value + Child.get(); }
    }
    interface OwnInterface {
        int VALUE = own(8);
        static int own(int n) { return n + 1; }
        static int read() { return VALUE + own(1); }
    }
    public static void main(String[] args) {
        check(initial == 4);
        check(coldLoop(0) == 0 && events == 0);
        check(coldLoop(1) == 3 && events == 129);
        check(coldLoop(1) == 3 && events == 12912);
        check(brokenLoop(0) == 0);
        try { brokenLoop(1); throw new AssertionError(); }
        catch (ExceptionInInitializerError expected) { check(events == 12913); }
        try { brokenLoop(1); throw new AssertionError(); }
        catch (NoClassDefFoundError expected) { check(events == 12913); }
        check(Child.inherited() == 14);
        check(OwnInterface.read() == 11);
        System.out.println("call checks passed " + events);
    }
}
