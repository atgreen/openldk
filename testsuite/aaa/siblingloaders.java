/* { dg-output "OK" } */

package aaa;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.util.function.Supplier;

// Two sibling class loaders each define a class named
// aaa.siblingloaders$HidXX, from different bytecode (GenX1 and GenX2,
// renamed; the names are the same length, so the class files stay valid).
// Each loader's class must keep its own static state, static methods and
// instance methods, and defining the second must not replace the first.
public class siblingloaders
{
    public static class GenX1 implements Supplier<String>
    {
        static int calls;
        static String who() { return "one"; }
        public String get() { return who() + (++calls); }
    }

    public static class GenX2 implements Supplier<String>
    {
        static int calls = 100;
        static String who() { return "two"; }
        public String get() { return who() + (++calls); }
    }

    static class Loader extends ClassLoader
    {
        final String source;

        Loader(String source)
        {
            super(siblingloaders.class.getClassLoader());
            this.source = source;
        }

        protected Class<?> findClass(String name) throws ClassNotFoundException
        {
            if (!name.equals("aaa.siblingloaders$HidXX"))
                throw new ClassNotFoundException(name);
            try (InputStream in = siblingloaders.class.getResourceAsStream("siblingloaders$" + source + ".class")) {
                ByteArrayOutputStream out = new ByteArrayOutputStream();
                for (int b; (b = in.read()) != -1; ) out.write(b);
                byte[] bytes = rename(out.toByteArray(), "siblingloaders$" + source, "siblingloaders$HidXX");
                return defineClass(name, bytes, 0, bytes.length);
            } catch (java.io.IOException e) {
                throw new ClassNotFoundException(name, e);
            }
        }

        static byte[] rename(byte[] b, String fromName, String toName)
        {
            byte[] from = fromName.getBytes();
            byte[] to = toName.getBytes();
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
    static Supplier<String> make(ClassLoader loader) throws Exception
    {
        Class<?> c = loader.loadClass("aaa.siblingloaders$HidXX");
        return (Supplier<String>) c.getDeclaredConstructor().newInstance();
    }

    public static void main(String[] args) throws Exception
    {
        Loader l1 = new Loader("GenX1");
        Supplier<String> a = make(l1);
        String a1 = a.get();
        Loader l2 = new Loader("GenX2");
        Supplier<String> b = make(l2);
        String b1 = b.get();
        String a2 = a.get();      // still loader 1's class, its own counter
        String a3 = make(l1).get();
        boolean distinct = a.getClass() != b.getClass();
        String got = a1 + " " + b1 + " " + a2 + " " + a3;
        boolean ok = got.equals("one1 two101 one2 one3") && distinct;
        System.out.println(ok ? "OK" : "FAIL: " + got + " distinct=" + distinct);
    }
}
