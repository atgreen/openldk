// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.lang.reflect.*;
import java.nio.file.*;

public class ReentrantCompilationProbe {
    static class Loader extends ClassLoader {
        final Path directory;
        Method callback;
        Object receiver;
        int callbacks;
        boolean denyTrigger;
        Loader(Path directory) { super(null); this.directory = directory; }
        @Override protected Class<?> findClass(String name) throws ClassNotFoundException {
            try {
                if (name.equals("Trigger") && denyTrigger)
                    throw new NoClassDefFoundError("expected literal linkage failure");
                if (name.equals("Trigger") && callback != null) {
                    boolean heldBefore = receiver != null && Thread.holdsLock(receiver);
                    if (!Integer.valueOf(7).equals(callback.invoke(receiver, false)))
                        throw new AssertionError("Reentrant callback result");
                    if (receiver != null && Thread.holdsLock(receiver) != heldBefore)
                        throw new AssertionError("Interpreted monitor leaked");
                    callbacks++;
                }
                byte[] bytes = Files.readAllBytes(directory.resolve(name + ".class"));
                return defineClass(name, bytes, 0, bytes.length);
            } catch (Exception error) { throw new ClassNotFoundException(name, error); }
        }
    }
    public static void main(String[] args) throws Exception {
        for (boolean instance : new boolean[]{false, true}) {
            Loader loader = new Loader(Path.of(args[0]));
            Class<?> hook = loader.loadClass("Hook");
            loader.receiver = instance ? hook.getConstructor().newInstance() : null;
            loader.callback = hook.getMethod(instance ? "instanceRun" : "staticRun", boolean.class);
            Object answer = loader.callback.invoke(loader.receiver, true);
            if (!Integer.valueOf(instance ? 49 : 42).equals(answer) || loader.callbacks != 1)
                throw new AssertionError("Outer compilation/callback did not complete: instance=" + instance
                    + " answer=" + answer + " callbacks=" + loader.callbacks);
        }
        Loader loader = new Loader(Path.of(args[0]));
        loader.denyTrigger = true;
        Method literal = loader.loadClass("Hook").getMethod("literal", boolean.class);
        if (literal.invoke(null, false) != String.class)
            throw new AssertionError("Untaken class literal caused a linkage error");
        try { literal.invoke(null, true); throw new AssertionError("Missing literal accepted"); }
        catch (InvocationTargetException expected) {
            if (!(expected.getCause() instanceof NoClassDefFoundError)) throw expected;
        }
        Method discard = loader.loadClass("Hook").getMethod("discardLiteral", boolean.class);
        discard.invoke(null, false);
        try { discard.invoke(null, true); throw new AssertionError("Unused literal linkage error eliminated"); }
        catch (InvocationTargetException expected) {
            if (!(expected.getCause() instanceof NoClassDefFoundError)) throw expected;
        }
        Loader arrayLoader = new Loader(Path.of(args[0]));
        Class<?> arrayType = (Class<?>) arrayLoader.loadClass("Hook").getMethod("arrayLiteral").invoke(null);
        if (arrayType.getClassLoader() != arrayLoader
            || arrayType.getComponentType().getComponentType() != arrayLoader.loadClass("Trigger"))
            throw new AssertionError("Array class literal loader identity");
        System.out.println("Reentrant class loading, arithmetic, exception handlers, and monitors passed; class literals resolve on use");
    }
}
