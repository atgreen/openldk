/* { dg-output "Result: 42.*Result: Hello, World.*MethodHandle test passed" } */

package aaa;

import java.lang.invoke.*;

public class MethodHandleTest {

    public static int getValue() {
        return 42;
    }

    public static String simpleMethod(String arg) {
        return "Hello, " + arg;
    }

    public static void main(String[] args) {
        try {
            // Test 1: Simple method with no arguments
            MethodHandles.Lookup lookup = MethodHandles.lookup();
            MethodType mt0 = MethodType.methodType(int.class);
            MethodHandle mh0 = lookup.findStatic(MethodHandleTest.class, "getValue", mt0);
            int result0 = (int) mh0.invokeExact();
            System.out.println("Result: " + result0);

            // Test 2: Method with String argument
            MethodType mt1 = MethodType.methodType(String.class, String.class);
            MethodHandle mh1 = lookup.findStatic(MethodHandleTest.class, "simpleMethod", mt1);
            String result1 = (String) mh1.invokeExact("World");
            System.out.println("Result: " + result1);

            System.out.println("MethodHandle test passed");
        } catch (Throwable t) {
            System.err.println("ERROR: " + t);
            t.printStackTrace();
            System.exit(1);
        }
    }
}
