// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.lang.reflect.Array;
public class ArrayReflectionProbe {
    interface Action { void run(); }
    static void check(boolean v) { if (!v) throw new AssertionError(); }
    static void expect(Class<? extends Throwable> type, Action action) {
        try { action.run(); throw new AssertionError("missing " + type); }
        catch (Throwable e) { if (!type.isInstance(e)) throw new AssertionError(e); }
    }
    public static void main(String[] args) {
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
