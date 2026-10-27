// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.net.*;
import java.nio.file.*;

public class HierarchyLoaderProbe {
    public static void main(String[] args) throws Exception {
        Path directory = Path.of(args[0]);
        try (URLClassLoader library = new URLClassLoader(new URL[]{directory.toUri().toURL()}, null)) {
            ClassLoader importer = new ClassLoader(null) {
                @Override protected Class<?> findClass(String name) throws ClassNotFoundException {
                    if (name.equals("Base") || name.equals("Leaf")) return library.loadClass(name);
                    if (name.equals("Caller")) {
                        try {
                            byte[] bytes = Files.readAllBytes(directory.resolve("Caller.class"));
                            return defineClass(name, bytes, 0, bytes.length);
                        } catch (Exception error) { throw new ClassNotFoundException(name, error); }
                    }
                    throw new ClassNotFoundException(name);
                }
            };
            try { importer.loadClass("Middle"); throw new AssertionError("Hidden superclass visible"); }
            catch (ClassNotFoundException expected) { }
            Class<?> caller = importer.loadClass("Caller");
            if (!Integer.valueOf(42).equals(caller.getMethod("staticValue").invoke(null))
                || !Integer.valueOf(37).equals(caller.getMethod("staticField").invoke(null)))
                throw new AssertionError("Inherited static member result");
            if (!"yes".equals(System.getProperty("hierarchy.base.initialized"))
                || System.getProperty("hierarchy.middle.initialized") != null
                || System.getProperty("hierarchy.leaf.initialized") != null)
                throw new AssertionError("Static access initialized a non-declaring class");
            Object value = caller.getMethod("make").invoke(null);
            if (!Integer.valueOf(42).equals(library.loadClass("Base").getMethod("value").invoke(value)))
                throw new AssertionError("Delegated hierarchy result");
        }
        System.out.println("Defining-loader verification hierarchy passed");
    }
}
