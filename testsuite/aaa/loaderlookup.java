/* { dg-output "OK" } */

package aaa;

import java.io.InputStream;
import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.List;

// Quarkus loads its application through its own class loader.  Code a
// loader defines must see itself as MethodHandles.lookup()'s lookup class
// (JBoss Logging finds generated loggers with Lookup.findClass), and the
// loader's own Java code, run while it defines classes, must not be asked
// for JDK classes or be given binary names.
public class loaderlookup
{
    public static class Plug
    {
        public static class Impl$logger {}

        static final String STATIC_LOOKUP = MethodHandles.lookup().lookupClass().getName();

        public static List<String> report() throws Exception
        {
            List<String> got = new ArrayList<>();
            MethodHandles.Lookup lookup = MethodHandles.lookup();
            got.add(STATIC_LOOKUP);
            got.add(lookup.lookupClass().getName());
            got.add(lookup.lookupClass().getClassLoader().getClass().getSimpleName());
            got.add(lookup.findClass("aaa.loaderlookup$Plug$Impl$logger").getName());
            got.add(Impl$logger[].class.getComponentType().getClassLoader().getClass().getSimpleName());
            return got;
        }
    }

    // Child-first for Plug and its nested classes, reading their class files
    // with Java I/O, as Quarkus's RunnerClassLoader does.
    static class Loader extends ClassLoader
    {
        Loader() { super(loaderlookup.class.getClassLoader()); }

        protected Class<?> loadClass(String name, boolean resolve) throws ClassNotFoundException
        {
            if (!name.startsWith("aaa.loaderlookup$Plug"))
                return super.loadClass(name, resolve);
            synchronized (getClassLoadingLock(name)) {
                Class<?> c = findLoadedClass(name);
                if (c != null)
                    return c;
                try (InputStream in = getParent().getResourceAsStream(name.replace('.', '/') + ".class")) {
                    if (in == null)
                        throw new ClassNotFoundException(name);
                    byte[] bytes = in.readAllBytes();
                    return defineClass(name, bytes, 0, bytes.length);
                } catch (java.io.IOException e) {
                    throw new ClassNotFoundException(name, e);
                }
            }
        }
    }

    public static void main(String[] args) throws Exception
    {
        Class<?> plug = Class.forName("aaa.loaderlookup$Plug", true, new Loader());
        Object got = plug.getMethod("report").invoke(null);
        String expected = "[aaa.loaderlookup$Plug, aaa.loaderlookup$Plug, Loader, aaa.loaderlookup$Plug$Impl$logger, Loader]";
        System.out.println(expected.equals(got.toString()) ? "OK" : got.toString());
    }
}
