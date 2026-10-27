// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.lang.reflect.Array;

public class ReflectionArrayProbe {
    interface Checked { void run(); }
    static void expect(Class<? extends Throwable> type, Checked operation) {
        try { operation.run(); }
        catch (Throwable error) {
            if (type.isInstance(error)) return;
            throw new AssertionError("Wrong exception", error);
        }
        throw new AssertionError("Missing " + type.getName());
    }
    public static void main(String[] args) {
        int[][] ints = (int[][]) Array.newInstance(int.class, 2, 3);
        if (ints.length != 2 || ints[0].length != 3 || ints[0][1] != 0 || ints[0] == ints[1])
            throw new AssertionError("Primitive dimensions or defaults");
        String[][][] partial = (String[][][]) Array.newInstance(String[].class, 2, 3);
        if (partial.getClass() != String[][][].class || partial[0][0] != null)
            throw new AssertionError("Array component class");
        ReflectionArrayProbe[][] own = (ReflectionArrayProbe[][]) Array.newInstance(ReflectionArrayProbe.class, 1, 2);
        if (own.getClass() != ReflectionArrayProbe[][].class || own[0][1] != null)
            throw new AssertionError("Application component identity");
        if (((double[]) Array.newInstance(double.class, 2))[1] != 0.0)
            throw new AssertionError("Primitive leaf defaults");
        expect(NullPointerException.class, () -> Array.newInstance(null, 1, 2));
        expect(NullPointerException.class, () -> Array.newInstance(int.class, (int[]) null));
        expect(IllegalArgumentException.class, () -> Array.newInstance(void.class, 1));
        expect(IllegalArgumentException.class, () -> Array.newInstance(int.class, new int[0]));
        expect(IllegalArgumentException.class, () -> Array.newInstance(int.class, new int[256]));
        Class<?> rank254 = Array.newInstance(int.class, new int[254]).getClass();
        expect(IllegalArgumentException.class, () -> Array.newInstance(rank254, 0, 0));
        expect(NegativeArraySizeException.class, () -> Array.newInstance(int.class, 0, -1));
        System.out.println("Reflective multidimensional arrays, defaults, identity, and validation passed");
    }
}
