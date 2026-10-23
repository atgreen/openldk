/* { dg-output "OK" } */

package aaa;

import java.util.*;

// Spring Boot finds the application class by walking the stack for main.
public class stackwalker
{
    static class Inner
    {
        static Class<?> caller()
        {
            return StackWalker.getInstance(StackWalker.Option.RETAIN_CLASS_REFERENCE).getCallerClass();
        }

        static Class<?> call()
        {
            return caller();
        }
    }

    public static void main(String[] args)
    {
        List<String> got = new ArrayList<>();
        got.add(StackWalker.getInstance().walk(s -> s.map(f -> f.getClassName() + "." + f.getMethodName()).toList()).toString());
        got.add(StackWalker.getInstance(StackWalker.Option.RETAIN_CLASS_REFERENCE)
                .walk(s -> s.filter(f -> f.getMethodName().equals("main")).findFirst()
                      .map(StackWalker.StackFrame::getDeclaringClass))
                .map(Class::getName).orElse("none"));
        got.add(Inner.call().getName());
        StackWalker.getInstance().forEach(f -> got.add(f.getMethodName()));
        String expected = "[[aaa.stackwalker.main], aaa.stackwalker, aaa.stackwalker$Inner, main]";
        System.out.println(expected.equals(got.toString()) ? "OK" : got.toString());
    }
}
