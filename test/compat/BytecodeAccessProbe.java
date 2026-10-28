// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
public class BytecodeAccessProbe {
    interface Action { void run() throws Exception; }
    static Object call() { return jdk.internal.misc.Unsafe.getUnsafe(); }
    static Class<?> literal() { return jdk.internal.misc.Unsafe.class; }
    static Object construct() throws Exception { return new sun.security.x509.X500Name("CN=test"); }
    static long field() { return jdk.internal.misc.Unsafe.ARRAY_INT_BASE_OFFSET; }
    static void denied(Action action) throws Exception {
        try { action.run(); throw new AssertionError("bytecode access allowed"); }
        catch (IllegalAccessError expected) { }
    }
    public static void main(String[] args) throws Exception {
        denied(() -> call()); denied(() -> literal()); denied(() -> construct()); denied(() -> field());
        // Repeated uses must continue to fail; denied resolution is not cached as success.
        denied(() -> call()); denied(() -> literal());
        System.out.println("Bytecode access: concealed classes rejected for calls, literals, allocation, and fields passed");
    }
}
