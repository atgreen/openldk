/* { dg-output "OK" } */

package aaa;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.util.function.Supplier;

// A class loader that, like ABCL's MemoryClassLoader, defines its classes
// only when findClass asks for them.  The Hid* classes exist nowhere else:
// the loader makes them from the Gen* class files by renaming them (Gen and
// Hid are the same length, so the class files stay valid).  HidA's code
// refers to HidB, and HidC's superclass is HidBase, so both can only be
// resolved by asking the loader.  Class.forName with the loader must go
// through its loadClass.
public class lazyloader
{
    public static class GenB
    {
        public static int VALUE = 41 + 1;
    }

    public static class GenA implements Supplier<String>
    {
        public String get() { return "A sees " + GenB.VALUE; }
    }

    public static class GenBase
    {
        public String base() { return "base"; }
    }

    public static class GenC extends GenBase implements Supplier<String>
    {
        public String get() { return "C extends " + base(); }
    }

    static class LazyLoader extends ClassLoader
    {
        int findCalls;

        LazyLoader() { super(lazyloader.class.getClassLoader()); }

        protected Class<?> findClass(String name) throws ClassNotFoundException
        {
            findCalls++;
            String prefix = "aaa.lazyloader$Hid";
            if (!name.startsWith(prefix))
                throw new ClassNotFoundException(name);
            String source = "lazyloader$Gen" + name.substring(prefix.length()) + ".class";
            try (InputStream in = lazyloader.class.getResourceAsStream(source)) {
                if (in == null)
                    throw new ClassNotFoundException(name);
                ByteArrayOutputStream out = new ByteArrayOutputStream();
                for (int b; (b = in.read()) != -1; ) out.write(b);
                byte[] bytes = rename(out.toByteArray());
                return defineClass(name, bytes, 0, bytes.length);
            } catch (java.io.IOException e) {
                throw new ClassNotFoundException(name, e);
            }
        }

        // Replace every "lazyloader$Gen" with "lazyloader$Hid".
        static byte[] rename(byte[] b)
        {
            byte[] from = "lazyloader$Gen".getBytes();
            byte[] to = "lazyloader$Hid".getBytes();
            outer:
            for (int i = 0; i + from.length <= b.length; i++) {
                for (int j = 0; j < from.length; j++)
                    if (b[i + j] != from[j]) continue outer;
                System.arraycopy(to, 0, b, i, to.length);
            }
            return b;
        }
    }

    @SuppressWarnings("unchecked")
    static String run(ClassLoader loader, String name) throws Exception
    {
        Class<?> c = loader.loadClass("aaa.lazyloader$" + name);
        if (c.getClassLoader() != loader)
            return name + " has the wrong loader";
        return ((Supplier<String>) c.getDeclaredConstructor().newInstance()).get();
    }

    public static void main(String[] args) throws Exception
    {
        LazyLoader loader = new LazyLoader();
        String a = run(loader, "HidA");
        String c = run(loader, "HidC");
        // Class.forName goes through loadClass: a class the loader already
        // defined comes back without another findClass call, and a JDK
        // class comes from the parent.
        int calls = loader.findCalls;
        boolean same = Class.forName("aaa.lazyloader$HidA", true, loader)
            == loader.loadClass("aaa.lazyloader$HidA")
            && loader.findCalls == calls;
        boolean parent = Class.forName("java.lang.String", false, loader) == String.class;
        boolean ok = a.equals("A sees 42") && c.equals("C extends base") && same && parent;
        System.out.println(ok ? "OK" : "FAIL: " + a + "; " + c + "; " + same + "; " + parent);
    }
}
