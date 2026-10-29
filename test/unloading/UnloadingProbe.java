// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.lang.invoke.*;
import java.lang.ref.*;
import java.lang.reflect.*;
import java.nio.file.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicReference;

public class UnloadingProbe {
    static Path directory;
    static class Child extends ClassLoader {
        Child() { super(ClassLoader.getSystemClassLoader()); }
        protected Class<?> findClass(String name) throws ClassNotFoundException {
            try {
                byte[] bytes = Files.readAllBytes(directory.resolve(name.replace('.', '/') + ".class"));
                return defineClass(name, bytes, 0, bytes.length);
            } catch (java.io.IOException e) { throw new ClassNotFoundException(name, e); }
        }
    }
    static class Tracked {
        final ReferenceQueue<Object> queue = new ReferenceQueue<>();
        final WeakReference<ClassLoader> loader;
        final WeakReference<Class<?>> type;
        Object root;
        Tracked(ClassLoader loader, Class<?> type, Object root) {
            this.loader = new WeakReference<>(loader, queue);
            this.type = new WeakReference<>(type, queue);
            this.root = root;
        }
    }
    static void check(boolean value, String message) { if (!value) throw new AssertionError(message); }
    static Tracked create(String kind) throws Throwable {
        Child loader = new Child();
        Class<?> type = loader.loadClass("payload.UnloadingPayload");
        Object root = switch (kind) {
            case "loader" -> loader;
            case "class" -> type;
            case "instance", "monitor" -> type.getConstructor().newInstance();
            case "array" -> Array.newInstance(type, 3);
            case "method" -> type.getMethod("answer");
            case "handle" -> MethodHandles.publicLookup().findStatic(type, "answer", MethodType.methodType(int.class));
            case "varhandle" -> MethodHandles.publicLookup().findStaticVarHandle(type, "counter", int.class);
            case "lambda" -> type.getMethod("supplier").invoke(null);
            case "dynamicLambda" -> type.getMethod("serializableSupplier").invoke(null);
            case "proxy" -> type.getMethod("proxy").invoke(null);
            case "lambdaClass" -> type.getMethod("supplier").invoke(null).getClass();
            case "uninitialized" -> null;
            default -> throw new AssertionError(kind);
        };
        if (!kind.equals("uninitialized")) {
            check(type.getMethod("answer").invoke(null).equals(42), "static invocation");
            check(type.getMethod("caller").invoke(null) == type, "caller-sensitive method identity");
            Object instance = type.getConstructor().newInstance();
            check(type.getMethod("instanceValue").invoke(instance).equals(42), "shadow field");
            check(type.getMethod("baseValue").invoke(instance).equals(31), "inherited field");
            check(instance.toString().equals("unloading payload"), "virtual invocation");
        }
        return new Tracked(loader, type, root);
    }
    static void alive(Tracked tracked, String kind) throws Throwable {
        for (int i = 0; i < 5; i++) { System.gc(); Thread.sleep(10); }
        check(tracked.loader.get() != null && tracked.type.get() != null, kind + " did not retain defining loader");
        switch (kind) {
            case "handle" -> check((int)((MethodHandle)tracked.root).invokeExact() == 42, "live handle");
            case "varhandle" -> check((int)((VarHandle)tracked.root).get() == 7, "live VarHandle");
            case "instance" -> check(tracked.root.getClass().getMethod("instanceValue").invoke(tracked.root).equals(42), "live instance");
            case "array" -> check(tracked.root.getClass().getComponentType() == tracked.type.get(), "live array component");
            case "lambda", "dynamicLambda" -> check(((java.util.function.Supplier<?>)tracked.root).get().equals(System.lineSeparator()), "live lambda");
            case "proxy" -> check(tracked.type.get().getMethod("callProxy", Object.class).invoke(null, tracked.root).equals(System.lineSeparator()), "live proxy");
            case "lambdaClass" -> check(((Class<?>)tracked.root).getClassLoader() == tracked.loader.get(), "lambda defining loader");
            case "monitor" -> { synchronized (tracked.root) { check(tracked.type.get() != null, "synchronized instance"); } }
        }
        Reference.reachabilityFence(tracked.root);
    }
    static void collected(Tracked tracked, String kind) throws Exception {
        if (Boolean.getBoolean("openldk.unloading.verbose")) System.err.println("Collecting " + kind);
        tracked.root = null;
        boolean queued = false;
        for (int i = 0; i < 150; i++) {
            System.gc();
            Reference<?> reference;
            while ((reference = tracked.queue.poll()) != null) if (reference == tracked.loader) queued = true;
            if (queued && tracked.loader.get() == null && tracked.type.get() == null) return;
            byte[][] pressure = new byte[8][];
            for (int j = 0; j < pressure.length; j++) pressure[j] = new byte[128 * 1024];
            Thread.sleep(20);
        }
        throw new AssertionError("loader/class not collected or queue not delivered: " + kind);
    }
    static Tracked startActive(CountDownLatch entered, CountDownLatch release, AtomicReference<Throwable> failure) throws Throwable {
        Child loader = new Child();
        Class<?> type = loader.loadClass("payload.UnloadingPayload");
        MethodHandle handle = MethodHandles.publicLookup().findStatic(type, "await",
            MethodType.methodType(int.class, CountDownLatch.class, CountDownLatch.class));
        Thread thread = new Thread(() -> {
            try { check((int)handle.invokeExact(entered, release) == 42, "active return"); }
            catch (Throwable e) { failure.set(e); }
        });
        thread.setDaemon(true); thread.start();
        return new Tracked(loader, type, thread);
    }
    public static void main(String[] args) throws Throwable {
        directory = Path.of(args[0]);
        var loading = java.lang.management.ManagementFactory.getClassLoadingMXBean();
        long unloadedBefore = loading.getUnloadedClassCount();
        boolean rootsOnly = args.length > 1 && args[1].equals("roots-only");
        for (String kind : new String[]{"loader", "class", "instance", "array", "method", "handle", "varhandle", "lambda", "dynamicLambda", "proxy", "lambdaClass", "monitor", "uninitialized"}) {
            Tracked tracked = create(kind);
            if (tracked.root != null) alive(tracked, kind);
            if (!rootsOnly) collected(tracked, kind);
        }
        if (!rootsOnly) {
            for (int i = 0; i < 12; i++) collected(create("instance"), "repeated " + i);
            CountDownLatch entered = new CountDownLatch(1), release = new CountDownLatch(1);
            AtomicReference<Throwable> failure = new AtomicReference<>();
            Tracked active = startActive(entered, release, failure);
            check(entered.await(10, TimeUnit.SECONDS), "active method did not enter");
            alive(active, "active");
            release.countDown();
            ((Thread)active.root).join(15000);
            check(!((Thread)active.root).isAlive(), "active method did not exit");
            if (failure.get() != null) throw failure.get();
            collected(active, "completed method");
            for (int i = 0; i < 100 && loading.getUnloadedClassCount() == unloadedBefore; i++) {
                System.gc(); Thread.sleep(20);
            }
            check(loading.getUnloadedClassCount() > unloadedBefore, "unloaded class accounting");
        }
        System.out.println(rootsOnly ? "Loader roots: live classes, instances, arrays and handles passed"
                                    : "Class unloading: roots, self cycles, repeated loaders and active methods passed");
    }
}
