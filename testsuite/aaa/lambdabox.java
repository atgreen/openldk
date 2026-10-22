/* { dg-output "OK" } */

package aaa;

import java.util.function.*;

// A lambda's implementation method returning a primitive, adapted to an
// interface method returning Object, boxes the result (Kotlin's lambdas
// are compiled this way: Function1.invoke returns Object).
public class lambdabox
{
    interface ObjFn { Object apply(Object x); }
    static boolean isEven(Object x) { return ((Integer) x) % 2 == 0; }
    static int twice(Object x) { return 2 * (Integer) x; }
    static char first(Object x) { return ((String) x).charAt(0); }

    public static void main(String[] args)
    {
        ObjFn even = lambdabox::isEven;
        ObjFn dbl = lambdabox::twice;
        ObjFn chr = lambdabox::first;
        Object b = even.apply(4), i = dbl.apply(21), c = chr.apply("xy");
        String result = (b instanceof Boolean) + " " + (i instanceof Integer) + " " + (c instanceof Character)
            + " " + b + " " + i + " " + c;
        System.out.println("true true true true 42 x".equals(result) ? "OK" : "FAIL: " + result);
    }
}
