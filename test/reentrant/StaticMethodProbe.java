// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.lang.invoke.*;

public class StaticMethodProbe {
    static int baseInitializations, childInitializations;
    public static class Base {
        static { baseInitializations++; }
        public static int value(int x) { return x + 41; }
    }
    public static class Child extends Base {
        static { childInitializations++; }
    }
    public static void main(String[] args) throws Throwable {
        MethodHandle inherited = MethodHandles.lookup().findStatic(Child.class, "value",
                MethodType.methodType(int.class, int.class));
        if (baseInitializations != 0 || childInitializations != 0)
            throw new AssertionError("lookup initialized classes");
        if ((int) inherited.invokeExact(1) != 42 || baseInitializations != 1 || childInitializations != 0)
            throw new AssertionError("inherited static method owner or initialization");
        System.out.println("Inherited static method handles initialize only their declaring class");
    }
}
