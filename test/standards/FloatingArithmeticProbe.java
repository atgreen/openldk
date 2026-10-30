// Copyright (C) 2026 Anthony Green <green@moxielogic.com>
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
public class FloatingArithmeticProbe {
    static long digest = 1;
    static int checks;
    static volatile Throwable threadFailure;
    static void record(double x) { digest = digest * 31 + Double.doubleToLongBits(x); checks++; }
    static void record(float x) { digest = digest * 31 + Float.floatToIntBits(x); checks++; }
    static double dself(double x) { return x - x; }
    static float fself(float x) { return x - x; }
    static void doubles(double x, double y) {
        record(x + y); record(x - y); record(x * y); record(x / y);
    }
    static void floats(float x, float y) {
        record(x + y); record(x - y); record(x * y); record(x / y);
    }
    static void doubleIdentities(double x) {
        record(x - x); record(x / x); record(x + 0d); record(x + -0d);
        record(x - 0d); record(0d - x); record(x * 0d); record(x * -0d);
        record(x / 0d); record(x / -0d); record(0d / x); record(-0d / x);
        record(StrictMath.sin(x)); record(StrictMath.cos(x)); record(StrictMath.tan(x));
        record(StrictMath.asin(x)); record(StrictMath.acos(x));
    }
    static void floatIdentities(float x) {
        record(x - x); record(x / x); record(x + 0f); record(x + -0f);
        record(x - 0f); record(0f - x); record(x * 0f); record(x * -0f);
        record(x / 0f); record(x / -0f); record(0f / x); record(-0f / x);
    }
    public static void main(String[] args) throws Exception {
        double nan = Double.longBitsToDouble(0x7fff800000000000L);
        if (!Double.isNaN(dself(nan)) || !Float.isNaN(fself((float)nan)))
            throw new AssertionError("NaN self-subtraction became zero");
        long[] dbits = {0, Long.MIN_VALUE, 0x3ff0000000000000L, 0xbff0000000000000L,
            0x7ff0000000000000L, 0xfff0000000000000L, 0x7ff8000000000000L,
            0x7ff0000000000001L, 0xfff8000000000001L, 0x7fff800000000000L,
            1, 2, Long.MIN_VALUE | 1, 0x0010000000000000L, 0x000fffffffffffffL,
            0x7fefffffffffffffL, 0xffefffffffffffffL, 0x3fb999999999999aL,
            0x3fefffffffffffffL, 0x3ff0000000000001L};
        int[] fbits = {0, Integer.MIN_VALUE, 0x3f800000, 0xbf800000,
            0x7f800000, 0xff800000, 0x7fc00000, 0x7f800001, 0xffc00001,
            1, 2, Integer.MIN_VALUE | 1, 0x00800000, 0x007fffff,
            0x7f7fffff, 0xff7fffff, 0x3dcccccd, 0x3f7fffff, 0x3f800001};
        for (long a : dbits) {
            double x = Double.longBitsToDouble(a);
            doubleIdentities(x);
            for (long b : dbits) doubles(x, Double.longBitsToDouble(b));
        }
        for (int a : fbits) {
            float x = Float.intBitsToFloat(a);
            floatIdentities(x);
            for (int b : fbits) floats(x, Float.intBitsToFloat(b));
        }
        Thread thread = new Thread(() -> {
            if (!Double.isNaN(dself(Double.POSITIVE_INFINITY)))
                throw new AssertionError("Thread floating-point mode");
        });
        thread.setUncaughtExceptionHandler((t, e) -> threadFailure = e);
        thread.start(); thread.join();
        if (threadFailure != null) throw new AssertionError(threadFailure);
        System.out.println("Floating arithmetic passed: " + checks + " " + Long.toHexString(digest));
    }
}
