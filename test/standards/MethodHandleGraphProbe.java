// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.lang.invoke.*;

public class MethodHandleGraphProbe {
    static int effects;
    public static MethodHandle bench;
    int value;
    public MethodHandleGraphProbe(int value) { this.value = value; }
    public int get() { return value; }
    static int add(int a, int b) { return a + b; }
    static int many(int a, int b, int c, int d, int e, int f, int g, int h, int i, int j) { return a+b+c+d+e+f+g+h+i+j; }
    static double mix(double x, long y) { return x + y; }
    static int twice(int a) { return a * 2; }
    static boolean positive(int a) { return a > 0; }
    static int risky(int a) { if (a < 0) throw new IllegalArgumentException(); return a; }
    static int recover(IllegalArgumentException e, int a) { return -a; }
    static void effect(int a) { effects += a; }
    static void check(int expected, int actual) {
        if (expected != actual) throw new AssertionError(expected + " != " + actual);
    }
    public static void main(String[] args) throws Throwable {
        var lookup = MethodHandles.lookup();
        var unary = MethodType.methodType(int.class, int.class);
        var add = lookup.findStatic(MethodHandleGraphProbe.class, "add", unary.appendParameterTypes(int.class));
        var twice = lookup.findStatic(MethodHandleGraphProbe.class, "twice", unary);
        var bound = MethodHandles.insertArguments(add, 0, 7);
        bench = bound;
        var filtered = MethodHandles.filterArguments(add, 0, twice, twice);
        var permuted = MethodHandles.permuteArguments(add, unary, 0, 0);
        var guard = MethodHandles.guardWithTest(
            lookup.findStatic(MethodHandleGraphProbe.class, "positive", unary.changeReturnType(boolean.class)),
            bound, twice);
        var caught = MethodHandles.catchException(
            lookup.findStatic(MethodHandleGraphProbe.class, "risky", unary), IllegalArgumentException.class,
            lookup.findStatic(MethodHandleGraphProbe.class, "recover", unary.insertParameterTypes(0, IllegalArgumentException.class)));
        var site = new MutableCallSite(unary);
        site.setTarget(bound);
        var dynamic = site.dynamicInvoker();
        for (int i = 0; i < 50; ++i) {
            check(i + 7, (int)bound.invokeExact(i));
            check(i * 6, (int)filtered.invokeExact(i, i * 2));
            check(i * 2, (int)permuted.invokeExact(i));
            check(i == 0 ? 0 : i + 7, (int)guard.invokeExact(i));
            check(i, (int)caught.invokeExact(-i));
            check(i + 7, (int)dynamic.invokeExact(i));
        }
        site.setTarget(twice);
        MutableCallSite.syncAll(new MutableCallSite[]{site});
        check(18, (int)dynamic.invokeExact(9));
        var constructor = lookup.findConstructor(MethodHandleGraphProbe.class, unary.changeReturnType(void.class));
        var object = (MethodHandleGraphProbe)constructor.invokeExact(42);
        var getter = lookup.findGetter(MethodHandleGraphProbe.class, "value", int.class).bindTo(object);
        check(42, (int)getter.invokeExact());
        var setter = lookup.findSetter(MethodHandleGraphProbe.class, "value", int.class).bindTo(object);
        setter.invokeExact(81);
        check(81, (int)getter.invokeExact());
        var virtual = lookup.findVirtual(MethodHandleGraphProbe.class, "get", MethodType.methodType(int.class)).bindTo(object);
        check(81, (int)virtual.invokeExact());
        var effect = lookup.findStatic(MethodHandleGraphProbe.class, "effect", unary.changeReturnType(void.class));
        var noArgs = MethodHandles.insertArguments(effect, 0, 13);
        noArgs.invokeExact();
        check(13, effects);
        var manyType = MethodType.methodType(int.class, int.class, int.class, int.class, int.class,
            int.class, int.class, int.class, int.class, int.class, int.class);
        var many = MethodHandles.insertArguments(lookup.findStatic(MethodHandleGraphProbe.class, "many", manyType), 0, 1);
        check(55, (int)many.invokeExact(2, 3, 4, 5, 6, 7, 8, 9, 10));
        var mix = lookup.findStatic(MethodHandleGraphProbe.class, "mix", MethodType.methodType(double.class, double.class, long.class));
        var mixedBound = MethodHandles.insertArguments(mix, 0, 1.5d);
        if ((double)mixedBound.invokeExact(8L) != 9.5d) throw new AssertionError("mixed primitive graph");
        var boxed = mixedBound.asType(MethodType.methodType(Object.class, Object.class));
        Object result = (Object)boxed.invokeExact((Object)Long.valueOf(8));
        if (!Double.valueOf(9.5d).equals(result)) throw new AssertionError("boxed graph");
        var staticGetter = MethodHandles.dropArguments(lookup.findStaticGetter(MethodHandleGraphProbe.class, "effects", int.class), 0, int.class);
        check(13, (int)staticGetter.invokeExact(99));
        System.out.println("method handle graphs passed");
    }
}
