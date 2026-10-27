// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.io.File;
import java.net.*;
import java.lang.reflect.Array;

public class ArrayLoaderProbe {
    static class Loader extends URLClassLoader {
        Loader(URL url) { super(new URL[]{url}, null); }
        @Override protected Class<?> loadClass(String name, boolean resolve) throws ClassNotFoundException {
            if (name.startsWith("[")) throw new AssertionError("Array descriptor passed to loadClass");
            if (name.equals("Broken")) throw new LinkageError("expected linkage failure");
            return super.loadClass(name, resolve);
        }
    }
    public static void main(String[] args) throws Exception {
        URL url = new File(args[0]).toURI().toURL();
        System.clearProperty("zip.payload.initialized");
        try (Loader first = new Loader(url); Loader second = new Loader(url)) {
            Class<?> a = Class.forName("[LZipPayload;", true, first);
            Class<?> b = Class.forName("[LZipPayload;", false, second);
            if (a == b || a.getClassLoader() != first || b.getClassLoader() != second)
                throw new AssertionError("Array class-loader identity");
            if (a != Class.forName("[LZipPayload;", false, first)
                || a.getComponentType() != first.loadClass("ZipPayload"))
                throw new AssertionError("Array component identity");
            Class<?> nested = Class.forName("[[LZipPayload;", true, first);
            if (nested.getClassLoader() != first || nested.getComponentType() != a)
                throw new AssertionError("Multidimensional array identity");
            if (Array.newInstance(a.getComponentType(), 0).getClass() != a)
                throw new AssertionError("Array object getClass identity");
            if (System.getProperty("zip.payload.initialized") != null)
                throw new AssertionError("Array lookup initialized component");
            if (Class.forName("[I", false, first) != int[].class)
                throw new AssertionError("Primitive array identity");
            try { Class.forName("[LAbsent;", false, first); throw new AssertionError("Missing component accepted"); }
            catch (ClassNotFoundException expected) { }
            try { Class.forName("Broken", false, first); throw new AssertionError("LinkageError swallowed"); }
            catch (LinkageError expected) { }
            try { Class.forName("ArrayLoaderProbe", false, first); throw new AssertionError("Null parent exposed application classpath"); }
            catch (ClassNotFoundException expected) { }
            try (URLClassLoader child = new URLClassLoader(new URL[0], ClassLoader.getSystemClassLoader())) {
                if (child.loadClass("ArrayLoaderProbe") != ArrayLoaderProbe.class)
                    throw new AssertionError("System-loader parent delegation");
            }
            if (Thread.holdsLock(first)) throw new AssertionError("Loader monitor leaked");
            Class.forName("ZipPayload", true, first);
            if (System.getProperty("zip.payload.initialized") == null)
                throw new AssertionError("First component not initialized");
            System.clearProperty("zip.payload.initialized");
            Class.forName("ZipPayload", true, second);
            if (System.getProperty("zip.payload.initialized") == null)
                throw new AssertionError("Initialization crossed loader namespaces");
            Object callbacks = first.loadClass("ZipPayload").getMethod("callbacks").invoke(null);
            Object otherCallbacks = second.loadClass("ZipPayload").getMethod("callbacks").invoke(null);
            Class<?> callbackType = first.loadClass("ZipCallback");
            if (callbacks.getClass().getComponentType() != callbackType
                || !callbackType.isInstance(Array.get(callbacks, 0))
                || callbackType.isInstance(Array.get(otherCallbacks, 0)))
                throw new AssertionError("Lambda interface loader identity");
            callbackType.getMethod("accept", int.class).invoke(Array.get(callbacks, 0), 7);
            if (!"7".equals(System.getProperty("zip.callback")))
                throw new AssertionError("Primitive lambda callback");
            Object supplier = first.loadClass("ZipPayload").getMethod("kindSupplier").invoke(null);
            Object kind = first.loadClass("ZipPayload$KindSupplier").getMethod("get").invoke(supplier);
            if (kind.getClass() != first.loadClass("ZipPayload$Kind"))
                throw new AssertionError("Lambda return type loader identity");
            Class<?> payload = first.loadClass("ZipPayload");
            if (payload.getModule() != first.getUnnamedModule()
                || payload.getModule().getClassLoader() != first
                || a.getModule() != payload.getModule())
                throw new AssertionError("Defining loader's unnamed module");
            if (Class.forName(payload.getModule(), "ZipPayload") != payload
                || Class.forName(payload.getModule(), "java.lang.String") != null
                || Class.forName(payload.getModule(), "Absent") != null)
                throw new AssertionError("Module-scoped class lookup");
            if (!"bundle resource".equals(payload.getMethod("resource").invoke(null)))
                throw new AssertionError("Caller-module resource bundle lookup");
        }
        System.out.println("Array loader identity, initialization, and failure propagation passed");
    }
}
