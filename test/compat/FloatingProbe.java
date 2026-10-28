// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
public class FloatingProbe {
    static float fadd(float a, float b) { return a + b; }
    static float fmul(float a, float b) { return a * b; }
    static float fdiv(float a, float b) { return a / b; }
    static float frem(float a, float b) { return a % b; }
    static double dadd(double a, double b) { return a + b; }
    static double dmul(double a, double b) { return a * b; }
    static double ddiv(double a, double b) { return a / b; }
    static double drem(double a, double b) { return a % b; }
    static float narrow(double x) { return (float)x; }
    static int floatToInt(float x) { return (int)x; }
    static long floatToLong(float x) { return (long)x; }
    static int toInt(double x) { return (int)x; }
    static long toLong(double x) { return (long)x; }
    static void f(float actual, float expected) {
        if (Float.floatToIntBits(actual) != Float.floatToIntBits(expected))
            throw new AssertionError("float " + Float.toHexString(actual) + " != " + Float.toHexString(expected));
    }
    static void d(double actual, double expected) {
        if (Double.doubleToLongBits(actual) != Double.doubleToLongBits(expected))
            throw new AssertionError("double " + Double.toHexString(actual) + " != " + Double.toHexString(expected));
    }
    static void check(boolean condition) { if (!condition) throw new AssertionError(); }
    public static void main(String[] args) {
        f(fadd(16777216f, 1f), 16777216f); d(dadd(9007199254740992d, 1d), 9007199254740992d);
        f(fmul(Float.MAX_VALUE, 2f), Float.POSITIVE_INFINITY);
        d(dmul(Double.MAX_VALUE, 2d), Double.POSITIVE_INFINITY);
        f(fdiv(1f, -0f), Float.NEGATIVE_INFINITY); f(fdiv(-1f, -0f), Float.POSITIVE_INFINITY);
        d(ddiv(1d, -0d), Double.NEGATIVE_INFINITY); d(ddiv(-1d, -0d), Double.POSITIVE_INFINITY);
        f(fdiv(0f, 0f), Float.NaN); d(ddiv(Double.POSITIVE_INFINITY, Double.POSITIVE_INFINITY), Double.NaN);
        f(fmul(Float.MIN_VALUE, 0.5f), 0f); d(dmul(Double.MIN_VALUE, 0.5d), 0d);
        f(fdiv(-Float.MIN_VALUE, 2f), -0f); d(ddiv(-Double.MIN_VALUE, 2d), -0d);
        f(fadd(Float.MIN_VALUE, Float.MIN_VALUE), Float.intBitsToFloat(2));
        d(dadd(Double.MIN_VALUE, Double.MIN_VALUE), Double.longBitsToDouble(2));
        f(frem(-4f, 2f), -0f); d(drem(-4d, 2d), -0d);
        f(frem(Float.MAX_VALUE, Float.MIN_VALUE), 0f); d(drem(Double.MAX_VALUE, Double.MIN_VALUE), 0d);
        f(frem(7f, Float.POSITIVE_INFINITY), 7f); d(drem(Double.POSITIVE_INFINITY, 2d), Double.NaN);
        f(narrow(Double.MAX_VALUE), Float.POSITIVE_INFINITY); f(narrow(-Double.MIN_VALUE), -0f);
        check(toInt(Double.NaN) == 0 && toLong(Double.NaN) == 0L);
        check(toInt(Double.POSITIVE_INFINITY) == Integer.MAX_VALUE && toInt(Double.NEGATIVE_INFINITY) == Integer.MIN_VALUE);
        check(toLong(Double.POSITIVE_INFINITY) == Long.MAX_VALUE && toLong(Double.NEGATIVE_INFINITY) == Long.MIN_VALUE);
        check(toInt(2147483648d) == Integer.MAX_VALUE && toLong(0x1p63) == Long.MAX_VALUE);
        check(toInt(-1.75d) == -1 && toLong(-1.75d) == -1L);
        check(floatToInt(-1.75f) == -1 && floatToLong(-1.75f) == -1L);
        check(floatToInt(Float.MAX_VALUE) == Integer.MAX_VALUE && floatToInt(-Float.MAX_VALUE) == Integer.MIN_VALUE);
        check(floatToLong(Float.MAX_VALUE) == Long.MAX_VALUE && floatToLong(-Float.MAX_VALUE) == Long.MIN_VALUE);
        check(!((0d / 0d) < 0) && !((0d / 0d) >= 0));
        System.out.println("Floating point: strict rounding, overflow, subnormals, signed zero, NaNs, remainder, and conversions passed");
    }
}
