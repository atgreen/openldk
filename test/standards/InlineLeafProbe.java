// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
public class InlineLeafProbe {
    static long digest = 1;
    static int effects;
    private static int add(int a, int b) { return a + b; }
    private static int subtract(int a, int b) { return a - b; }
    private static int multiply(int a, int b) { return a * b; }
    private static int bits(int a, int b) { return (a & b) ^ (a | b); }
    private static int negate(int a) { return -a; }
    private static int narrow(int a) { return (byte)a + (short)a + (char)a; }
    private static int literal() { return -129; }
    private static int divide(int a, int b) { return a / b; }
    private static synchronized int synchronizedAdd(int a, int b) { return a + b; }
    static int effect(int a) { ++effects; return a; }
    static void record(int a) { digest = digest * 31 + a; }
    static class Cold {
        static { effects += 10; }
        static int add(int a, int b) { return a + b; }
    }
    public static int loop(int n) {
        int sum = 0;
        for (int i = 0; i < n; ++i) sum = add(sum, i);
        return sum;
    }
    public static void main(String[] args) {
        int[] values = {0, 1, -1, Integer.MIN_VALUE, Integer.MAX_VALUE, 128, 65535, -65536};
        for (int a : values) {
            record(negate(a)); record(narrow(a));
            for (int b : values) {
                record(add(a,b)); record(subtract(a,b)); record(multiply(a,b)); record(bits(a,b));
                record(synchronizedAdd(a,b));
            }
        }
        record(literal()); record(loop(0)); record(loop(1000));
        record(Cold.add(effect(1), effect(2)));
        if (effects != 12) throw new AssertionError("class initialization order");
        try { divide(effect(1), effect(0)); throw new AssertionError("missing division error"); }
        catch (ArithmeticException expected) { if (effects != 14) throw new AssertionError("argument order"); }
        System.out.println("inline leaves passed " + digest);
    }
}
