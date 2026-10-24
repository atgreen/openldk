/* { dg-output "OK" } */

package aaa;

import java.util.*;

// try-with-resources inside a try/catch: javac's inner catch-all closes the
// resource and rethrows, across several blocks, all inside the outer range.
// The rethrow must reach the outer catch.
public class twrcatch
{
    static List<String> log = new ArrayList<>();

    static class R implements AutoCloseable
    {
        final String name;
        final boolean failClose;
        R(String name, boolean failClose) { this.name = name; this.failClose = failClose; }
        public void close() { log.add("close " + name); if (failClose) throw new IllegalArgumentException("close " + name); }
    }

    static void plain()
    {
        try (R r = new R("a", false)) {
            throw new IllegalStateException("body");
        } catch (IllegalStateException e) {
            log.add("caught " + e.getMessage() + " suppressed=" + e.getSuppressed().length);
        }
    }

    static void suppressed()
    {
        try (R r = new R("b", true)) {
            throw new IllegalStateException("body");
        } catch (IllegalStateException e) {
            log.add("caught " + e.getMessage() + " suppressed=" + e.getSuppressed()[0].getMessage());
        }
    }

    static void nested()
    {
        try (R outer = new R("c", false); R inner = new R("d", false)) {
            throw new UnsupportedOperationException("nested");
        } catch (UnsupportedOperationException e) {
            log.add("caught " + e.getMessage());
        }
    }

    static void inLoop()
    {
        for (int i = 0; i < 3; i++) {
            try (R r = new R("e" + i, false)) {
                if (i % 2 == 0)
                    throw new IllegalStateException("loop" + i);
                log.add("body e" + i);
            } catch (IllegalStateException e) {
                log.add("caught " + e.getMessage());
            }
        }
    }

    public static void main(String[] args)
    {
        plain();
        suppressed();
        nested();
        inLoop();
        String expected = "[close a, caught body suppressed=0, close b, caught body suppressed=close b, "
            + "close d, close c, caught nested, close e0, caught loop0, body e1, close e1, close e2, caught loop2]";
        System.out.println(expected.equals(log.toString()) ? "OK" : log.toString());
    }
}
