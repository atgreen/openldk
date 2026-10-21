/* { dg-output "OK" } */

package aaa;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.util.function.Supplier;

// An interface defined by one class loader, implemented by a class that a
// sibling loader defines -- the shape of Clojure's per-evaluation
// DynamicClassLoaders (a defprotocol, then a defrecord implementing it).
// The class must be an instance of the interface.  (HidI and HidC are made
// from GenI and GenC by renaming; the names are the same length.)
public class crossloader
{
    public interface GenI { String who(); }

    public static class GenC implements GenI, Supplier<String>
    {
        public String who() { return "C"; }
        public String get() { return ((GenI) (Object) this).who() + " as I"; }
    }

    static byte[] bytes(String source) throws Exception
    {
        try (InputStream in = crossloader.class.getResourceAsStream("crossloader$" + source + ".class")) {
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            for (int b; (b = in.read()) != -1; ) out.write(b);
            byte[] b = out.toByteArray();
            byte[] from = "crossloader$Gen".getBytes(), to = "crossloader$Hid".getBytes();
            outer:
            for (int i = 0; i + from.length <= b.length; i++) {
                for (int j = 0; j < from.length; j++)
                    if (b[i + j] != from[j]) continue outer;
                System.arraycopy(to, 0, b, i, to.length);
            }
            return b;
        }
    }

    // Defines HidI only.
    static class InterfaceLoader extends ClassLoader
    {
        InterfaceLoader() { super(crossloader.class.getClassLoader()); }

        protected Class<?> findClass(String name) throws ClassNotFoundException
        {
            if (!name.equals("aaa.crossloader$HidI")) throw new ClassNotFoundException(name);
            try { byte[] b = bytes("GenI"); return defineClass(name, b, 0, b.length); }
            catch (Exception e) { throw new ClassNotFoundException(name, e); }
        }
    }

    // Defines HidC; gets HidI from the other loader, which is not its parent.
    static class ClassLoaderB extends ClassLoader
    {
        final ClassLoader interfaces;

        ClassLoaderB(ClassLoader interfaces)
        {
            super(crossloader.class.getClassLoader());
            this.interfaces = interfaces;
        }

        protected Class<?> loadClass(String name, boolean resolve) throws ClassNotFoundException
        {
            synchronized (getClassLoadingLock(name)) {
                if (name.equals("aaa.crossloader$HidI"))
                    return interfaces.loadClass(name);
                if (name.equals("aaa.crossloader$HidC")) {
                    Class<?> c = findLoadedClass(name);
                    if (c != null) return c;
                    try { byte[] b = bytes("GenC"); return defineClass(name, b, 0, b.length); }
                    catch (Exception e) { throw new ClassNotFoundException(name, e); }
                }
                return super.loadClass(name, resolve);
            }
        }
    }

    @SuppressWarnings("unchecked")
    public static void main(String[] args) throws Exception
    {
        ClassLoader a = new InterfaceLoader();
        Class<?> iface = a.loadClass("aaa.crossloader$HidI");
        ClassLoader b = new ClassLoaderB(a);
        Class<?> impl = b.loadClass("aaa.crossloader$HidC");
        Object o = impl.getDeclaredConstructor().newInstance();
        boolean isInstance = iface.isInstance(o);
        boolean assignable = iface.isAssignableFrom(impl);
        String got = ((Supplier<String>) o).get();
        boolean ok = isInstance && assignable && got.equals("C as I");
        System.out.println(ok ? "OK" : "FAIL: isInstance=" + isInstance + " assignable=" + assignable + " got=" + got);
    }
}
