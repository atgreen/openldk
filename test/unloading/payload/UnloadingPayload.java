// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
package payload;
import java.util.concurrent.CountDownLatch;
public class UnloadingPayload extends UnloadingBase {
    public int value = 42;
    public static volatile int counter = 7;
    public static Object self = new Object[] {UnloadingPayload.class, new UnloadingPayload(), UnloadingPayload.class.getClassLoader()};
    public interface Action { String get(); }
    public static java.util.function.Supplier<String> supplier() { return System::lineSeparator; }
    public static java.util.function.Supplier<String> serializableSupplier() {
        return (java.util.function.Supplier<String> & java.io.Serializable) System::lineSeparator;
    }
    public static Object proxy() throws ReflectiveOperationException {
        var target = java.lang.invoke.MethodHandles.publicLookup().findStatic(System.class, "lineSeparator",
            java.lang.invoke.MethodType.methodType(String.class));
        return java.lang.invoke.MethodHandleProxies.asInterfaceInstance(Action.class, target);
    }
    public static String callProxy(Object value) { return ((Action)value).get(); }
    public int instanceValue() { return value; }
    public static Class<?> caller() { return java.lang.invoke.MethodHandles.lookup().lookupClass(); }
    public static int answer() { return 42; }
    public static int await(CountDownLatch started, CountDownLatch release) throws InterruptedException {
        started.countDown(); release.await(); return 42;
    }
    public String toString() { return "unloading payload"; }
}
