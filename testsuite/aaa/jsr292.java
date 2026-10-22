/* { dg-output "OK" } */

package aaa;

import java.lang.invoke.*;
import java.util.*;
import static java.lang.invoke.MethodHandles.*;
import static java.lang.invoke.MethodType.methodType;

// JSR-292 method handles: lookups, combinators, adapters, call sites.
// OpenLDK runs method handles itself rather than generating LambdaForm
// bytecode (see src/native/jsr292.lisp).
public class jsr292 {
    static int add(int a, int b) { return a + b; }
    static String cat(String a, String b) { return a + b; }
    static String hello(String who) { return "hello " + who; }
    static boolean isEven(int x) { return x % 2 == 0; }
    static String even(int x) { return x + " even"; }
    static String odd(int x) { return x + " odd"; }
    static int boom(int x) { throw new IllegalStateException("boom " + x); }
    static int recover(IllegalStateException e, int x) { return -x; }
    static String join(String... parts) { return String.join("+", parts); }
    static int len(String s) { return s.length(); }
    static long twice(long x) { return 2 * x; }
    static double half(double d) { return d / 2; }
    static String withChar(char c, boolean b) { return c + ":" + b; }
    int field = 7;
    static String sfield = "S";
    String name;
    jsr292() { name = "default"; }
    jsr292(String n) { name = n; }
    String greet(String g) { return g + " " + name; }

    static int failures = 0;
    static void check(String what, Object got, Object want) {
        boolean ok = Objects.equals(got, want);
        if (!ok) failures++;
        if (!ok) System.out.println("FAIL " + what + " = " + got + " (want " + want + ")");
    }
    interface Body { Object run() throws Throwable; }
    static void test(String what, Object want, Body b) {
        Object got;
        try { got = b.run(); } catch (Throwable t) { got = "threw " + t; }
        check(what, got, want);
    }

    public static void main(String[] args) throws Throwable {
        Lookup L = lookup();
        MethodHandle add = L.findStatic(jsr292.class, "add", methodType(int.class, int.class, int.class));
        MethodHandle cat = L.findStatic(jsr292.class, "cat", methodType(String.class, String.class, String.class));
        MethodHandle hello = L.findStatic(jsr292.class, "hello", methodType(String.class, String.class));
        MethodHandle len = L.findStatic(jsr292.class, "len", methodType(int.class, String.class));

        test("invokeExact", 5, () -> (int) add.invokeExact(2, 3));
        test("invoke boxed", 5, () -> (Integer) add.invoke((Object) 2, (Object) 3));
        test("invokeWithArguments", 9, () -> add.invokeWithArguments(4, 5));
        test("insertArguments", 11, () -> (int) insertArguments(add, 0, 10).invokeExact(1));
        test("bindTo", "hello world", () -> (String) hello.bindTo("world").invokeExact());
        test("dropArguments", "ab", () -> (String) dropArguments(cat, 1, int.class).invokeExact("a", 42, "b"));
        test("filterArguments", "hello x!", () -> (String) filterArguments(cat, 0, hello).invokeExact("x", "!"));
        test("filterReturnValue", 7, () -> (int) filterReturnValue(hello, len).invokeExact("x"));
        test("foldArguments", "hello aa", () -> (String) foldArguments(cat, hello).invokeExact("a"));
        test("permuteArguments", "ba", () -> (String) permuteArguments(cat, methodType(String.class, String.class, String.class), 1, 0).invokeExact("a", "b"));
        MethodHandle gwt = guardWithTest(
            L.findStatic(jsr292.class, "isEven", methodType(boolean.class, int.class)),
            L.findStatic(jsr292.class, "even", methodType(String.class, int.class)),
            L.findStatic(jsr292.class, "odd", methodType(String.class, int.class)));
        test("guardWithTest even", "4 even", () -> (String) gwt.invokeExact(4));
        test("guardWithTest odd", "5 odd", () -> (String) gwt.invokeExact(5));
        MethodHandle safe = catchException(
            L.findStatic(jsr292.class, "boom", methodType(int.class, int.class)), IllegalStateException.class,
            L.findStatic(jsr292.class, "recover", methodType(int.class, IllegalStateException.class, int.class)));
        test("catchException", -3, () -> (int) safe.invokeExact(3));
        test("constant", "k", () -> (String) constant(String.class, "k").invokeExact());
        test("identity", 6, () -> (int) identity(int.class).invokeExact(6));
        test("asType box", "hello q", () -> hello.asType(methodType(Object.class, Object.class)).invokeExact((Object) "q"));
        test("asType int->Integer", 3, () -> (Integer) add.asType(methodType(Integer.class, Integer.class, Integer.class)).invokeExact((Integer) 1, (Integer) 2));
        MethodHandle join = L.findStatic(jsr292.class, "join", methodType(String.class, String[].class));
        test("varargs invoke", "a+b+c", () -> (String) join.invoke("a", "b", "c"));
        test("varargs invokeWithArguments", "x+y", () -> join.invokeWithArguments("x", "y"));
        test("asCollector", "p+q", () -> (String) join.asCollector(String[].class, 2).invokeExact("p", "q"));
        test("asSpreader", 7, () -> (int) add.asSpreader(Object[].class, 2).invokeExact(new Object[] { 3, 4 }));
        MethodHandle greet = L.findVirtual(jsr292.class, "greet", methodType(String.class, String.class));
        test("findVirtual", "hi default", () -> (String) greet.invokeExact(new jsr292(), "hi"));
        test("findConstructor", "bob", () -> ((jsr292) L.findConstructor(jsr292.class, methodType(void.class, String.class)).invokeExact("bob")).name);
        test("findGetter", 7, () -> (int) L.findGetter(jsr292.class, "field", int.class).invokeExact(new jsr292()));
        test("findStaticGetter", "S", () -> (String) L.findStaticGetter(jsr292.class, "sfield", String.class).invokeExact());
        test("findSetter", 9, () -> { jsr292 m = new jsr292(); L.findSetter(jsr292.class, "field", int.class).invokeExact(m, 9); return m.field; });
        test("unreflect", "yo default", () -> (String) L.unreflect(jsr292.class.getDeclaredMethod("greet", String.class)).invokeExact(new jsr292(), "yo"));
        test("long", 10L, () -> (long) L.findStatic(jsr292.class, "twice", methodType(long.class, long.class)).invokeExact(5L));
        test("double", 1.5, () -> (double) L.findStatic(jsr292.class, "half", methodType(double.class, double.class)).invokeExact(3.0));
        test("char/boolean", "z:true", () -> (String) L.findStatic(jsr292.class, "withChar", methodType(String.class, char.class, boolean.class)).invokeExact('z', true));
        MutableCallSite site = new MutableCallSite(methodType(String.class, String.class));
        site.setTarget(hello);
        MethodHandle dyn = site.dynamicInvoker();
        test("MutableCallSite", "hello m", () -> (String) dyn.invokeExact("m"));
        site.setTarget(dropArguments(constant(String.class, "changed"), 0, String.class));
        test("MutableCallSite retarget", "changed", () -> (String) dyn.invokeExact("m"));
        VolatileCallSite vsite = new VolatileCallSite(hello);
        test("VolatileCallSite", "hello v", () -> (String) vsite.dynamicInvoker().invokeExact("v"));
        test("ConstantCallSite", "hello c", () -> (String) new ConstantCallSite(hello).dynamicInvoker().invokeExact("c"));
        SwitchPoint sp = new SwitchPoint();
        MethodHandle guarded = sp.guardWithTest(hello, dropArguments(constant(String.class, "fallback"), 0, String.class));
        test("SwitchPoint valid", "hello s", () -> (String) guarded.invokeExact("s"));
        SwitchPoint.invalidateAll(new SwitchPoint[] { sp });
        test("SwitchPoint invalidated", "fallback", () -> (String) guarded.invokeExact("s"));
        test("exactInvoker", 3, () -> (int) exactInvoker(add.type()).invokeExact(add, 1, 2));
        test("invoker", 3, () -> (int) invoker(add.type()).invokeExact(add, 1, 2));
        test("arrayElementGetter", "b", () -> (String) arrayElementGetter(String[].class).invokeExact(new String[] { "a", "b" }, 1));
        test("arrayLength", 2, () -> (int) arrayLength(int[].class).invokeExact(new int[] { 1, 2 }));
        test("throwException", "threw java.lang.IllegalArgumentException: t",
             () -> (String) throwException(String.class, IllegalArgumentException.class).invokeExact(new IllegalArgumentException("t")));
        test("zero", 0, () -> (int) zero(int.class).invokeExact());
        test("empty", null, () -> (String) empty(methodType(String.class, int.class)).invokeExact(1));
        MethodHandle tf = tryFinally(hello, dropArguments(dropArguments(identity(String.class), 0, Throwable.class), 2, String.class));
        test("tryFinally", "hello f", () -> (String) tf.invokeExact("f"));
        MethodHandle counted = countedLoop(constant(int.class, 4),
                                           constant(int.class, 0),
                                           add.asType(methodType(int.class, int.class, int.class)));
        test("countedLoop", 6, () -> (int) counted.invokeExact());
        test("explicitCastArguments", 65, () -> (int) explicitCastArguments(identity(char.class), methodType(int.class, char.class)).invokeExact('A'));
        test("toString", "MethodHandle(int,int)int", () -> add.toString());
        System.out.println(failures == 0 ? "OK" : failures + " FAILED");
    }
}
