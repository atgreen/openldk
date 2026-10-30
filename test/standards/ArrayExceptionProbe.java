// Copyright (C) 2026 Anthony Green <green@moxielogic.com>
//
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.util.Arrays;

public class ArrayExceptionProbe {
    static int checks;
    interface Action { void run() throws Exception; }
    static void check(boolean value) {
        checks++;
        if (!value) throw new AssertionError("check " + checks);
    }
    static void expect(Class<? extends Throwable> type, Action action) {
        checks++;
        try { action.run(); }
        catch (Throwable e) {
            if (e.getClass() == type) return;
            throw new AssertionError("expected " + type.getName() + ", got " + e, e);
        }
        throw new AssertionError("missing " + type.getName());
    }
    static Object load(Object a, int kind, int i) {
        return switch (kind) {
            case 0 -> ((boolean[]) a)[i]; case 1 -> ((byte[]) a)[i];
            case 2 -> ((char[]) a)[i]; case 3 -> ((short[]) a)[i];
            case 4 -> ((int[]) a)[i]; case 5 -> ((long[]) a)[i];
            case 6 -> ((float[]) a)[i]; default -> ((double[]) a)[i];
        };
    }
    static void store(Object a, int kind, int i) {
        switch (kind) {
            case 0 -> ((boolean[]) a)[i] = true; case 1 -> ((byte[]) a)[i] = 1;
            case 2 -> ((char[]) a)[i] = 'a'; case 3 -> ((short[]) a)[i] = 1;
            case 4 -> ((int[]) a)[i] = 1; case 5 -> ((long[]) a)[i] = 1;
            case 6 -> ((float[]) a)[i] = 1; default -> ((double[]) a)[i] = 1;
        }
    }
    static class Plain {
        Object copy() throws CloneNotSupportedException { return super.clone(); }
    }
    static class Copyable extends Plain implements Cloneable {
        int value = 17;
        Object field = new Object();
    }
    static void cloneChecks() throws Exception {
        expect(CloneNotSupportedException.class, () -> new Plain().copy());
        Copyable original = new Copyable();
        Copyable copy = (Copyable) original.copy();
        check(copy != original && copy.getClass() == original.getClass());
        check(copy.value == 17 && copy.field == original.field);
        int[] ints = {1, 2};
        int[] intsCopy = ints.clone();
        check(intsCopy != ints && Arrays.equals(ints, intsCopy));
        Object[] objects = {original};
        Object[] objectsCopy = objects.clone();
        check(objectsCopy != objects && objectsCopy[0] == original);
    }
    static void boundsChecks(Object[] arrays) {
        for (int kind = 0; kind < 8; kind++) {
            final int k = kind;
            for (int index : new int[]{-1, -21767345, Integer.MIN_VALUE, 2, Integer.MAX_VALUE}) {
                expect(ArrayIndexOutOfBoundsException.class, () -> load(arrays[k], k, index));
                expect(ArrayIndexOutOfBoundsException.class, () -> store(arrays[k], k, index));
            }
            expect(NullPointerException.class, () -> load(null, k, -1));
            expect(NullPointerException.class, () -> store(null, k, -1));
            store(arrays[k], k, 0);
            check(load(arrays[k], k, 0) != null);
        }
        expect(ArrayIndexOutOfBoundsException.class, () -> { int[] a = new int[2]; a[-21767345] = 9; });
        expect(ArrayIndexOutOfBoundsException.class, () -> { int[] a = new int[2]; int x = a[-21767345]; });
    }
    static void copyChecks(Object[] arrays) {
        // Primitive kinds must match even for an empty copy. In particular,
        // boolean and byte share Lisp storage but are distinct Java types.
        for (int a = 0; a < arrays.length; a++) for (int b = 0; b < arrays.length; b++) {
            Object src = arrays[a], dest = arrays[b];
            if (a == b) { System.arraycopy(src, 0, dest, 0, 0); check(true); }
            else expect(ArrayStoreException.class, () -> System.arraycopy(src, 0, dest, 0, 0));
        }
        expect(NullPointerException.class, () -> System.arraycopy(null, -1, new Object(), -1, -1));
        expect(NullPointerException.class, () -> System.arraycopy(new Object(), -1, null, -1, -1));
        expect(ArrayStoreException.class, () -> System.arraycopy(new Object(), -1, new int[2], -1, -1));
        expect(ArrayStoreException.class, () -> System.arraycopy(new byte[2], -1, new boolean[2], -1, -1));
        Object[] src = {"first", "second", 42, "last"};
        String[] dest = {"a", "b", "c", "d", "e"};
        expect(ArrayStoreException.class, () -> System.arraycopy(src, 0, dest, 1, 4));
        check(Arrays.equals(dest, new String[]{"a", "first", "second", "d", "e"}));
        System.arraycopy(src, 2, dest, 0, 0); // incompatible elements are not inspected
        for (int[] range : new int[][]{{-1,0,1}, {0,-1,1}, {0,0,-1}, {3,0,2}, {0,4,2}, {Integer.MAX_VALUE,0,1}, {0,0,Integer.MAX_VALUE}}) {
            String[] before = dest.clone();
            expect(ArrayIndexOutOfBoundsException.class,
                () -> System.arraycopy(src, range[0], dest, range[1], range[2]));
            check(Arrays.equals(before, dest));
        }
        int[] numbers = {0,1,2,3,4};
        System.arraycopy(numbers, 0, numbers, 1, 4);
        check(Arrays.equals(numbers, new int[]{0,0,1,2,3}));
        System.arraycopy(numbers, 1, numbers, 0, 4);
        check(Arrays.equals(numbers, new int[]{0,1,2,3,3}));
        String[] words = {"a","b","c"};
        System.arraycopy(words, 0, words, 1, 2);
        check(Arrays.equals(words, new String[]{"a","a","b"}));
        Number[] nums = new Number[2];
        System.arraycopy(new Integer[]{1,2}, 0, nums, 0, 2);
        check(nums[0].intValue() == 1 && nums[1].intValue() == 2);
        Object[][] nested = new Object[2][];
        System.arraycopy(new String[][]{{"x"}, {"y"}}, 0, nested, 0, 2);
        check(nested[0][0].equals("x") && nested[1][0].equals("y"));
    }
    public static void main(String[] args) throws Exception {
        Object[] arrays = {new boolean[2], new byte[2], new char[2], new short[2],
            new int[2], new long[2], new float[2], new double[2], new Object[2]};
        cloneChecks(); boundsChecks(arrays); copyChecks(arrays);
        System.out.println("ArrayExceptionProbe " + checks + " passed");
    }
}
