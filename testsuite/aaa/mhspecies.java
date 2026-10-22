/* { dg-output "OK" } */

package aaa;

import java.lang.invoke.*;
import static java.lang.invoke.MethodHandles.*;
import static java.lang.invoke.MethodType.methodType;

// Bound method handles whose species the JDK doesn't pregenerate (long,
// float, many arguments, mixed types), MethodHandleProxies, and VarHandles.
public class mhspecies
{
    static String seven(String a, String b, String c, String d, String e, String f, String g) { return a+b+c+d+e+f+g; }
    static long lsum(long a, int b) { return a + b; }
    static float fmul(float a, float b) { return a * b; }
    static String mixed(long a, double b, int c, String d) { return a + "/" + b + "/" + c + "/" + d; }
    public interface Adder { int add(int a, int b); }
    static int plus(int a, int b) { return a + b; }
    static volatile int counter;

    public static void main(String[] args) throws Throwable
    {
        Lookup L = lookup();
        MethodHandle seven = L.findStatic(mhspecies.class, "seven", methodType(String.class, String.class, String.class, String.class, String.class, String.class, String.class, String.class));
        StringBuilder sb = new StringBuilder();
        sb.append((String) insertArguments(seven, 0, "a", "b", "c", "d", "e", "f").invokeExact("g")).append(' ');
        sb.append((long) insertArguments(L.findStatic(mhspecies.class, "lsum", methodType(long.class, long.class, int.class)), 0, 40L).invokeExact(2)).append(' ');
        sb.append((float) insertArguments(L.findStatic(mhspecies.class, "fmul", methodType(float.class, float.class, float.class)), 0, 1.5f).invokeExact(4f)).append(' ');
        sb.append((String) insertArguments(L.findStatic(mhspecies.class, "mixed", methodType(String.class, long.class, double.class, int.class, String.class)), 0, 1L, 2.5, 3).invokeExact("z")).append(' ');
        Adder a = MethodHandleProxies.asInterfaceInstance(Adder.class, L.findStatic(mhspecies.class, "plus", methodType(int.class, int.class, int.class)));
        sb.append(a.add(20, 22)).append(' ');
        VarHandle vh = L.findStaticVarHandle(mhspecies.class, "counter", int.class);
        vh.getAndAdd(5);
        vh.compareAndSet(5, 7);
        sb.append(counter);
        String result = sb.toString();
        System.out.println("abcdefg 42 6.0 1/2.5/3/z 42 7".equals(result) ? "OK" : "FAIL: " + result);
    }
}
