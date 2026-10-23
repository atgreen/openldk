/* { dg-output "OK" } */

package aaa;

import java.lang.annotation.*;
import java.lang.reflect.*;
import java.util.*;

// Method.invoke boxes a primitive result by the method's return type.
// ByteBuddy reads annotation members this way (an int member must come
// back an Integer), on the annotation's proxy.
public class reflectbox
{
    @Retention(RetentionPolicy.RUNTIME)
    @interface A { int i(); byte b(); short s(); long l(); char c(); boolean z(); float f(); double d(); int dflt() default 7; }

    @A(i = -1, b = 2, s = 3, l = 4, c = 'x', z = false, f = 1.5f, d = 2.5) static class T {}

    interface I { int f(); }

    static int plain() { return 3; }
    static void nothing() {}

    public static void main(String[] args) throws Exception
    {
        A ann = T.class.getAnnotation(A.class);
        List<String> got = new ArrayList<>();
        for (String n : List.of("i", "b", "s", "l", "c", "z", "f", "d", "dflt")) {
            Object v = A.class.getMethod(n).invoke(ann);
            got.add(n + "=" + v + ":" + v.getClass().getSimpleName());
        }
        Object proxy = Proxy.newProxyInstance(I.class.getClassLoader(), new Class<?>[] { I.class }, (p, m, a) -> 5);
        Object v = I.class.getMethod("f").invoke(proxy);
        got.add(v.getClass().getSimpleName());
        v = reflectbox.class.getDeclaredMethod("plain").invoke(null);
        got.add(v.getClass().getSimpleName());
        got.add(String.valueOf(reflectbox.class.getDeclaredMethod("nothing").invoke(null)));
        String expected = "[i=-1:Integer, b=2:Byte, s=3:Short, l=4:Long, c=x:Character, z=false:Boolean, "
            + "f=1.5:Float, d=2.5:Double, dflt=7:Integer, Integer, Integer, null]";
        System.out.println(expected.equals(got.toString()) ? "OK" : got.toString());
    }
}
