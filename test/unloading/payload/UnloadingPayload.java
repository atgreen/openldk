// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
package payload;
import java.util.concurrent.CountDownLatch;
public class UnloadingPayload extends UnloadingBase {
    public int value = 42;
    public static volatile int counter = 7;
    public static Object self = new Object[] {UnloadingPayload.class, new UnloadingPayload(), UnloadingPayload.class.getClassLoader()};
    public int instanceValue() { return value; }
    public static Class<?> caller() { return java.lang.invoke.MethodHandles.lookup().lookupClass(); }
    public static int answer() { return 42; }
    public static int await(CountDownLatch started, CountDownLatch release) throws InterruptedException {
        started.countDown(); release.await(); return 42;
    }
    public String toString() { return "unloading payload"; }
}
