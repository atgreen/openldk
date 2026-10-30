// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.lang.ref.*;
import java.nio.file.*;

public class UnloadingJNIProbe {
    static Path directory;
    static native void releasePin();
    static class Child extends ClassLoader {
        Child() { super(ClassLoader.getSystemClassLoader()); }
        protected Class<?> findClass(String name) throws ClassNotFoundException {
            try {
                byte[] bytes = Files.readAllBytes(directory.resolve(name.replace('.', '/') + ".class"));
                return defineClass(name, bytes, 0, bytes.length);
            } catch (java.io.IOException e) { throw new ClassNotFoundException(name, e); }
        }
    }
    static WeakReference<ClassLoader> load(String path, boolean pin) throws Throwable {
        Child loader = new Child();
        Class<?> type = loader.loadClass("payload.UnloadingNative");
        type.getMethod("load", String.class, boolean.class).invoke(null, path, pin);
        if (!type.getMethod("version").invoke(null).equals(25)) throw new AssertionError("version");
        return new WeakReference<>(loader);
    }
    static String events(Path log) throws Exception { return Files.exists(log) ? Files.readString(log) : ""; }
    static void collect(WeakReference<ClassLoader> weak, Path log, int count) throws Exception {
        for (int i = 0; i < 200; i++) {
            System.gc(); Thread.sleep(20);
            String text = events(log);
            if (text.contains("BAD")) throw new AssertionError(text);
            long unloads = text.chars().filter(ch -> ch == 'U').count();
            if (weak.get() == null && unloads == count) return;
        }
        throw new AssertionError("JNI loader not released or OnUnload missing: " + events(log));
    }
    public static void main(String[] args) throws Throwable {
        directory = Path.of(args[0]);
        Path log = Path.of(System.getenv("OPENLDK_UNLOAD_LOG"));
        for (int cycle = 1; cycle <= 3; cycle++) {
            boolean pin = cycle == 2;
            WeakReference<ClassLoader> weak = load(args[1], pin);
            if (pin) {
                for (int i = 0; i < 5; i++) { System.gc(); Thread.sleep(20); }
                if (weak.get() == null || events(log).chars().filter(ch -> ch == 'U').count() != cycle - 1)
                    throw new AssertionError("JNI global reference did not retain loader");
                releasePin();
            }
            collect(weak, log, cycle);
        }
        String text = events(log);
        if (text.chars().filter(ch -> ch == 'L').count() != 3 || text.chars().filter(ch -> ch == 'U').count() != 3)
            throw new AssertionError("JNI load/unload counts: " + text);
        System.out.println("JNI unloading: global/weak references, IDs, OnUnload and same-path reload passed");
    }
}
