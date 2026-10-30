// Copyright (C) 2026 Anthony Green <green@moxielogic.com>
//
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.lang.reflect.*;

public class ReflectionConversionProbe {
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
    public boolean z; public byte b; public short s; public char c;
    public int i; public long j; public float f; public double d;
    public String text;
    public static int staticInt;
    public static boolean z(boolean x) { return x; }
    public static byte b(byte x) { return x; }
    public static short s(short x) { return x; }
    public static char c(char x) { return x; }
    public static int i(int x) { return x; }
    public static long j(long x) { return x; }
    public static float f(float x) { return x; }
    public static double d(double x) { return x; }
    public String reference(String x) { return x; }
    public void fail() { throw new IllegalStateException("target"); }
    public static class Constructed {
        public int value;
        public Constructed(int value) { this.value = value; }
    }
    static boolean widens(int from, int to) {
        if (from == to) return true;
        return switch (from) {
            case 1 -> to == 2 || to >= 4;
            case 2, 3 -> to >= 4;
            case 4 -> to >= 5;
            case 5 -> to >= 6;
            case 6 -> to == 7;
            default -> false;
        };
    }
    public static void main(String[] args) throws Exception {
        Class<?>[] types = {boolean.class, byte.class, short.class, char.class,
            int.class, long.class, float.class, double.class};
        Class<?>[] boxes = {Boolean.class, Byte.class, Short.class, Character.class,
            Integer.class, Long.class, Float.class, Double.class};
        Object[] values = {false, (byte)7, (short)7, (char)7, 7, 7L, 7f, 7d};
        String[] names = {"z", "b", "s", "c", "i", "j", "f", "d"};
        ReflectionConversionProbe instance = new ReflectionConversionProbe();
        for (int target = 0; target < 8; target++) {
            Object array = Array.newInstance(types[target], 2);
            Field field = ReflectionConversionProbe.class.getField(names[target]);
            Method method = ReflectionConversionProbe.class.getMethod(names[target], types[target]);
            check(Array.get(array, 0).getClass() == boxes[target]);
            if (target == 0) check(Array.get(array, 0).equals(Boolean.FALSE));
            for (int source = 0; source < 8; source++) {
                Object value = values[source];
                Object[] supplied = {value};
                if (widens(source, target)) {
                    Array.set(array, 0, value);
                    field.set(instance, value);
                    Object result = method.invoke(new Object(), supplied); // static ignores receiver
                    check(result.getClass() == boxes[target]);
                    check(result.equals(Array.get(array, 0)));
                    check(result.equals(field.get(instance)));
                    check(supplied[0] == value); // invocation must not unbox in place
                } else {
                    Object before = Array.get(array, 0), fieldBefore = field.get(instance);
                    expect(IllegalArgumentException.class, () -> Array.set(array, 0, value));
                    expect(IllegalArgumentException.class, () -> field.set(instance, value));
                    expect(IllegalArgumentException.class, () -> method.invoke(null, supplied));
                    check(before.equals(Array.get(array, 0)) && fieldBefore.equals(field.get(instance)));
                }
            }
            expect(IllegalArgumentException.class, () -> Array.set(array, 0, null));
            expect(IllegalArgumentException.class, () -> field.set(instance, null));
            expect(IllegalArgumentException.class, () -> method.invoke(null, new Object[]{null}));
            expect(IllegalArgumentException.class, () -> method.invoke(null, new Object[0]));
            expect(IllegalArgumentException.class, () -> method.invoke(null, values));
        }
        String[] strings = {"original"};
        expect(IllegalArgumentException.class, () -> Array.set(strings, 0, new Object()));
        check(strings[0].equals("original"));
        Array.set(strings, 0, null); check(strings[0] == null);
        expect(NullPointerException.class, () -> Array.get(null, 0));
        expect(IllegalArgumentException.class, () -> Array.get(new Object(), 0));
        expect(ArrayIndexOutOfBoundsException.class, () -> Array.set(strings, -1, new Object()));
        Field field = ReflectionConversionProbe.class.getField("text");
        expect(IllegalArgumentException.class, () -> field.set(instance, new Object()));
        expect(IllegalArgumentException.class, () -> field.get(new Object()));
        expect(IllegalArgumentException.class, () -> field.set(new Object(), "x"));
        expect(NullPointerException.class, () -> field.get(null));
        field.set(instance, "ok"); check(field.get(instance).equals("ok"));
        field.set(instance, null); check(field.get(instance) == null);
        Field stat = ReflectionConversionProbe.class.getField("staticInt");
        stat.set(new Object(), (short)3); check(stat.get(null).equals(3));
        Method ref = ReflectionConversionProbe.class.getMethod("reference", String.class);
        expect(IllegalArgumentException.class, () -> ref.invoke(instance, new Object()));
        expect(IllegalArgumentException.class, () -> ref.invoke(new Object(), "x"));
        expect(NullPointerException.class, () -> ref.invoke(null, "x"));
        check(ref.invoke(instance, "x").equals("x"));
        Method fail = ReflectionConversionProbe.class.getMethod("fail");
        try { fail.invoke(instance); throw new AssertionError(); }
        catch (InvocationTargetException e) { check(e.getCause() instanceof IllegalStateException); }
        Constructor<Constructed> ctor = Constructed.class.getConstructor(int.class);
        check(ctor.newInstance((short)7).value == 7);
        check(ctor.newInstance((char)7).value == 7);
        expect(IllegalArgumentException.class, () -> ctor.newInstance(7L));
        expect(IllegalArgumentException.class, () -> ctor.newInstance(new Object[]{null}));
        expect(IllegalArgumentException.class, () -> ctor.newInstance());
        expect(IllegalArgumentException.class, () -> ctor.newInstance(1,2));
        System.out.println("ReflectionConversionProbe " + checks + " passed");
    }
}
