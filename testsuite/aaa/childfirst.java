/* { dg-output "OK" } */

package aaa;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.util.function.Supplier;

// A child-first class loader overrides loadClass to define classes itself
// before asking its parent.  Shadow is on the classpath, but the loader
// defines its own Shadow (from Impost's bytecode, renamed; the names are the
// same length, so the class file stays valid), and its own copy of User.
// User's reference to Shadow must resolve through the loader, to its Shadow.
public class childfirst
{
    public static class Shadow
    {
        public static String who() { return "parent"; }
    }

    public static class Impost
    {
        public static String who() { return "child"; }
    }

    public static class User implements Supplier<String>
    {
        public String get() { return Shadow.who(); }
    }

    static class ChildFirst extends ClassLoader
    {
        ChildFirst() { super(childfirst.class.getClassLoader()); }

        protected Class<?> loadClass(String name, boolean resolve) throws ClassNotFoundException
        {
            synchronized (getClassLoadingLock(name)) {
                Class<?> c = findLoadedClass(name);
                if (c == null) {
                    if (name.equals("aaa.childfirst$Shadow"))
                        c = define(name, "Impost");
                    else if (name.equals("aaa.childfirst$User"))
                        c = define(name, "User");
                    else
                        return super.loadClass(name, resolve);
                }
                return c;
            }
        }

        Class<?> define(String name, String source) throws ClassNotFoundException
        {
            try (InputStream in = childfirst.class.getResourceAsStream("childfirst$" + source + ".class")) {
                ByteArrayOutputStream out = new ByteArrayOutputStream();
                for (int b; (b = in.read()) != -1; ) out.write(b);
                byte[] bytes = rename(out.toByteArray());
                return defineClass(name, bytes, 0, bytes.length);
            } catch (java.io.IOException e) {
                throw new ClassNotFoundException(name, e);
            }
        }

        // Replace every "childfirst$Impost" with "childfirst$Shadow".
        static byte[] rename(byte[] b)
        {
            byte[] from = "childfirst$Impost".getBytes();
            byte[] to = "childfirst$Shadow".getBytes();
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
    public static void main(String[] args) throws Exception
    {
        ChildFirst loader = new ChildFirst();
        Class<?> user = loader.loadClass("aaa.childfirst$User");
        String inLoader = ((Supplier<String>) user.getDeclaredConstructor().newInstance()).get();
        String outside = new User().get();
        boolean ok = inLoader.equals("child") && outside.equals("parent")
            && user.getClassLoader() == loader;
        System.out.println(ok ? "OK" : "FAIL: " + inLoader + " " + outside);
    }
}
