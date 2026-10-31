// Copyright (C) 2026 Anthony Green <green@moxielogic.com>
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.lang.reflect.Array;
public class ArrayReflectionProbe {
    interface Action { void run(); }
    static void check(boolean v) { if (!v) throw new AssertionError(); }
    static void expect(Class<? extends Throwable> type, Action action) {
        try { action.run(); throw new AssertionError("missing " + type); }
        catch (Throwable e) { if (!type.isInstance(e)) throw new AssertionError(e); }
    }
    static int arrayKind(Object array) {
        return switch (array) {
            case byte[] x -> 0;
            case char[] x -> 1;
            case short[] x -> 2;
            case int[] x -> 3;
            case float[] x -> 4;
            case long[] x -> 5;
            case double[] x -> 6;
            case boolean[] x -> 7;
            default -> -1;
        };
    }
    public static void main(String[] args) {
        Object[] arrays = {new byte[1], new char[1], new short[1], new int[1],
                           new float[1], new long[1], new double[1], new boolean[1]};
        for (int target = 0; target < arrays.length; target++) {
            Class<?> type = arrays[target].getClass();
            for (int value = 0; value < arrays.length; value++)
                check(type.isInstance(arrays[value]) == (target == value));
            check(!type.isInstance(null));
            check(!type.isInstance("not an array"));
            check(!type.isInstance(new Object()));
            check(arrayKind(arrays[target]) == target);
        }
        check(Object[].class.isInstance(new String[1]));
        check(!String[].class.isInstance(new Object[1]));
        check(!Object[].class.isInstance(new int[1]));
        check(Object[].class.isInstance(new int[1][1]));
        check(int[][].class.isInstance(new int[1][1]));
        check(!long[][].class.isInstance(new int[1][1]));
        check(Cloneable.class.isInstance(new int[1]));
        check(java.io.Serializable.class.isInstance(new int[1]));
        check(!int.class.isInstance(Integer.valueOf(1)));
        expect(ClassCastException.class, () -> byte[].class.cast(new float[1]));
        boolean[] z = new boolean[1]; Array.setBoolean(z, 0, true); check(Array.getBoolean(z, 0));
        byte[] b = new byte[1]; Array.setByte(b, 0, (byte)-7); check(Array.getByte(b, 0) == -7);
        short[] s = new short[1]; Array.setByte(s, 0, (byte)-7); check(Array.getShort(s, 0) == -7);
        char[] c = new char[1]; Array.setChar(c, 0, '\uffff'); check(Array.getChar(c, 0) == '\uffff');
        int[] i = new int[1]; Array.setChar(i, 0, '\uffff'); check(Array.getInt(i, 0) == 65535);
        long[] j = new long[1]; Array.setInt(j, 0, Integer.MAX_VALUE); check(Array.getLong(j, 0) == Integer.MAX_VALUE);
        float[] f = new float[1]; Array.setLong(f, 0, 123); check(Array.getFloat(f, 0) == 123.0f);
        double[] d = new double[1]; Array.setFloat(d, 0, -0.0f); check(Double.doubleToRawLongBits(Array.getDouble(d, 0)) == Long.MIN_VALUE);
        check(Array.getDouble(c, 0) == 65535.0);
        expect(IllegalArgumentException.class, () -> Array.setInt(b, 0, 1));
        expect(IllegalArgumentException.class, () -> Array.getByte(i, 0));
        expect(IllegalArgumentException.class, () -> Array.getInt(z, 0));
        expect(IllegalArgumentException.class, () -> Array.getLength("not an array"));
        expect(IllegalArgumentException.class, () -> Array.setInt(new Object[1], 0, 1));
        expect(NullPointerException.class, () -> Array.getInt(null, 0));
        expect(ArrayIndexOutOfBoundsException.class, () -> Array.getInt(i, 2));
        System.out.println("Array reflection: primitive access, widening, signed zero and errors passed");
    }
}
