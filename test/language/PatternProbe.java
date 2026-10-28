// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
public class PatternProbe {
    sealed interface Shape permits Point, Box {}
    record Point(int x, int y) implements Shape {}
    record Box(Shape item) implements Shape {}
    enum Color { RED, GREEN }
    enum OtherColor { RED }
    record Bad(int value) { public int value() { throw new IllegalStateException("accessor-boom"); } }
    static void check(boolean ok) { if (!ok) throw new AssertionError(); }
    static int classify(Shape shape) {
        return switch (shape) {
            case null -> -1;
            case Point(int x, int y) when x < 0 -> y;
            case Point(int x, int y) -> x + y;
            case Box(Point(int x, int y)) -> x * y;
            case Box(var item) -> 99;
        };
    }
    public static void main(String[] args) {
        check(classify(null) == -1 && classify(new Point(20, 22)) == 42);
        check(classify(new Point(-1, 7)) == 7);
        check(classify(new Box(new Point(6, 7))) == 42);
        check(classify(new Box(null)) == 99);
        Object p = new Point(20, 22);
        check(p instanceof Point(int x, int y) && x + y == 42);
        Object text = "abc";
        check(text instanceof String s && s.length() == 3);
        check(switch (text) { case String s when s.isEmpty() -> false; case String s -> s.equals("abc"); default -> false; });
        check(switch (Color.GREEN) { case RED -> false; case GREEN -> true; });
        for (Object color : new Object[]{Color.RED, Color.GREEN, OtherColor.RED}) {
            int expected = color == Color.RED ? 1 : color == Color.GREEN ? 2 : 3;
            check(switch(color) { case Color.RED -> 1; case Color.GREEN -> 2; case OtherColor.RED -> 3; default -> 0; } == expected);
        }
        try { Shape missing = null; int ignored = switch(missing) { case Point pt -> 1; case Box b -> 2; }; throw new AssertionError(ignored); }
        catch (NullPointerException expected) { }
        try { Object bad = new Bad(1); if (bad instanceof Bad(int n)) throw new AssertionError(n); }
        catch (MatchException expected) { check(expected.getCause() instanceof IllegalStateException); }
        System.out.println("Patterns: record deconstruction, sealed exhaustiveness, guards, restart, nulls, enums, and accessor failures passed");
    }
}
