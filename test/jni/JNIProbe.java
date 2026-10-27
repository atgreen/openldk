// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.nio.ByteBuffer;

public class JNIProbe {
    int value;
    static long total = 12;
    JNIProbe(int value) { this.value = value; }
    int plus(int n) { return value + n; }
    static double sum(int n, float f, double d) { return n + f + d; }
    static void fail() { throw new IllegalStateException("callback failure"); }
    static void shutdownDone() { System.out.println("JNI worker finished"); }
    static int attachedIdentity() {
        Thread thread = Thread.currentThread();
        if (!thread.isAlive() || thread.isDaemon() || thread.getName().equals("main"))
            System.err.println("Attached thread: " + thread.getName() + " alive=" + thread.isAlive() + " daemon=" + thread.isDaemon());
        return thread.isAlive() && !thread.isDaemon() && !thread.getName().equals("main") ? 28 : -1;
    }
    static int daemonIdentity() {
        Thread thread = Thread.currentThread();
        if (!thread.isAlive() || !thread.isDaemon() || !thread.getName().equals("JNI daemon"))
            System.err.println("Daemon thread: " + thread.getName() + " alive=" + thread.isAlive() + " daemon=" + thread.isDaemon());
        return thread.isAlive() && thread.isDaemon() && thread.getName().equals("JNI daemon") ? 14 : -1;
    }
    static native int registered(int n);
    static native boolean echoBoolean(boolean n);
    static native byte echoByte(byte n);
    static native char echoChar(char n);
    static native short echoShort(short n);
    static native int echoInt(int n);
    static native long echoLong(long n);
    static native float echoFloat(float n);
    static native double echoDouble(double n);
    static native Object echoObject(Object n);
    static native int overloaded(int n);
    static native long overloaded(long n);
    static native String strings(String text);
    static native int[] arrays(int[] values);
    static native int callbacks();
    native int fields(int n);
    static native void raise();
    static native void propagate();
    static native boolean clearException();
    static native void remember(Object object);
    static native Object recall();
    static native void forget();
    static native int attachedThread();
    static native void missing();
    static native int cpp(int n);
    static native int café();
    static native int 𐐀();
    static native boolean advanced();
    static native ByteBuffer direct();
    static native int directCheck(ByteBuffer buffer);
    static native boolean reflection(java.lang.reflect.Method method, java.lang.reflect.Field field);
    static synchronized native boolean synchronizedCall();
    static native Class<?> define(byte[] bytes, ClassLoader loader);

    static void check(boolean result, String message) {
        if (!result) throw new AssertionError(message);
    }
    static class OwnerLoader extends ClassLoader {
        OwnerLoader() { super(null); }
        Class<?> owner(byte[] bytes) { return defineClass(null, bytes, 0, bytes.length); }
    }

    public static void main(String[] args) throws Exception {
        try { System.loadLibrary("openldk_jni_missing_library"); throw new AssertionError("missing library accepted"); }
        catch (UnsatisfiedLinkError expected) { }
        if (args.length > 2 && args[2].equals("runtime")) {
            Runtime.getRuntime().load(args[0]);
            Runtime.getRuntime().loadLibrary("jniprobe");
        } else {
            System.load(args[0]);
        }
        System.load(args[0]); // repeated load in the same loader is harmless
        try { System.load(args[1] + "/libjniprobe_bad.so"); throw new AssertionError("bad JNI version accepted"); }
        catch (UnsatisfiedLinkError expected) { }
        Class<?> owner = new OwnerLoader().owner(java.nio.file.Files.readAllBytes(
                java.nio.file.Path.of(args[1], "LibraryOwner.class")));
        check((boolean)owner.getMethod("load", String.class).invoke(null, args[0]),
              "library loaded in two class loaders");
        byte[] twinBytes = java.nio.file.Files.readAllBytes(java.nio.file.Path.of(args[1], "NativeTwin.class"));
        Class<?> firstTwin = new OwnerLoader().owner(twinBytes);
        Class<?> secondTwin = new OwnerLoader().owner(twinBytes);
        firstTwin.getMethod("load", String.class).invoke(null, args[1] + "/libjniprobe_twin1.so");
        secondTwin.getMethod("load", String.class).invoke(null, args[1] + "/libjniprobe_twin2.so");
        check((int)firstTwin.getMethod("value").invoke(null) == 1 && (int)secondTwin.getMethod("value").invoke(null) == 2,
              "native resolution crossed class-loader namespaces");
        check(registered(7) == 21, "RegisterNatives/JNI_OnLoad");
        check(echoBoolean(true) && !echoBoolean(false), "boolean ABI");
        check(echoByte((byte)-117) == -117, "byte ABI");
        check(echoChar('\ufffe') == '\ufffe', "char ABI");
        check(echoShort((short)-30000) == -30000, "short ABI");
        check(echoInt(-2000000000) == -2000000000, "int ABI");
        check(echoLong(-9000000000000000000L) == -9000000000000000000L, "long ABI");
        check(echoFloat(-3.25f) == -3.25f && Float.isNaN(echoFloat(Float.NaN)), "float ABI");
        check(echoDouble(-7.125) == -7.125 && Double.isInfinite(echoDouble(Double.POSITIVE_INFINITY)), "double ABI");
        Object object = new Object();
        check(echoObject(object) == object && echoObject(null) == null, "object ABI");
        check(overloaded(8) == 9 && overloaded(8L) == 10L, "overloaded symbol lookup");
        String unicode = "A\u0000\u00e9\u20ac\ud83d\ude00\ud800";
        check(unicode.equals(strings(unicode)), "modified UTF-8 and UTF-16");
        int[] data = {1, -2, 300};
        int[] result = arrays(data);
        check(data[0] == 11 && data[1] == 22 && data[2] == 300, "array copy-back/commit/abort");
        check(result.length == 2 && result[0] == -7 && result[1] == 42, "new array/region");
        check(callbacks() == 48, "constructors and A/V/variadic callbacks");
        JNIProbe probe = new JNIProbe(10);
        check(probe.fields(5) == 15 && probe.value == 15 && total == 99, "fields and monitor");
        try { raise(); throw new AssertionError("missing native exception"); }
        catch (IllegalArgumentException expected) { check(expected.getMessage().equals("from JNI"), "exception message"); }
        try { propagate(); throw new AssertionError("missing callback exception"); }
        catch (IllegalStateException expected) { }
        check(clearException(), "pending exception clear");
        remember(object);
        System.gc();
        check(recall() == object, "global reference survived GC");
        forget();
        check(recall() == null, "deleted global reference");
        check(attachedThread() == 28, "foreign-thread attachment and callback");
        check(café() == 31 && 𐐀() == 32, "JNI Unicode symbol escaping");
        check(advanced(), "primitive arrays/reference frames/weak refs");
        check(synchronizedCall(), "synchronized native method");
        ByteBuffer buffer = direct();
        check(buffer.isDirect() && buffer.capacity() == 16, "direct buffer creation");
        buffer.put(2, (byte)55);
        check(directCheck(buffer) == 55, "direct buffer address");
        check(reflection(JNIProbe.class.getDeclaredMethod("sum", int.class, float.class, double.class),
                         JNIProbe.class.getDeclaredField("value")), "reflection and stable member IDs");
        ClassLoader definedLoader = new ClassLoader() { };
        Class<?> defined = define(java.nio.file.Files.readAllBytes(java.nio.file.Path.of(args[1], "JNIDefined.class")), definedLoader);
        check(defined.getClassLoader() == definedLoader && (int)defined.getMethod("answer").invoke(null) == 42,
              "DefineClass with explicit loader");
        try { missing(); throw new AssertionError("missing native accepted"); }
        catch (UnsatisfiedLinkError expected) { }
        System.setProperty("java.library.path", args[1]);
        System.loadLibrary("jniprobe_cpp");
        check(cpp(6) == 42, "C++ JNIEnv interface");
        System.out.println("JNI probe passed");
    }
}
