// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
public class PrimitivePatternProbe {
    static void check(boolean ok) { if (!ok) throw new AssertionError(); }
    static String classify(Object value) {
        return switch (value) {
            case byte b -> "byte:" + b;
            case int i -> "int:" + i;
            case double d -> "double:" + d;
            case null -> "null";
            default -> "other";
        };
    }
    static boolean boxed(Integer value) {
        return switch(value) { case float f -> f == 42; case int i -> false; };
    }
    static boolean integral(long value) {
        return switch(value) { case int i -> i == 42; case long l -> l == Long.MAX_VALUE; };
    }
    static boolean floating(double value) {
        return switch(value) {
            case 0d -> Double.doubleToRawLongBits(value) == 0;
            case int i -> i == 42;
            case float f -> Float.isNaN(f) || f == Float.POSITIVE_INFINITY || Float.floatToRawIntBits(f) == 0x80000000;
            case double d -> d == 0.1d;
        };
    }
    public static void main(String[] args) {
        check(classify((byte)42).equals("byte:42"));
        check(classify(42).equals("int:42"));
        check(classify(1000).equals("int:1000"));
        check(classify(1.5d).equals("double:1.5"));
        check(classify(null).equals("null") && classify("x").equals("other"));
        check(boxed(42) && !boxed(16_777_217));
        check(integral(42) && integral(Long.MAX_VALUE));
        check(floating(0d) && floating(-0d) && floating(42d) && floating(Double.NaN)
                && floating(Double.POSITIVE_INFINITY) && floating(0.1d));
        int narrow = 127; check(narrow instanceof byte b && b == 127);
        int wide = 128; check(!(wide instanceof byte));
        double fractional = 1.5; check(!(fractional instanceof int));
        double integral = 42; check(integral instanceof int i && i == 42);
        long big = 16_777_217L; check(!(big instanceof float));
        check(switch(1.5d) { case 0d -> false; case double d when d > 1d -> true; default -> false; });
        System.out.println("Primitive patterns: exact conversions, boxed switches, narrowing boundaries, and guards passed");
    }
}
