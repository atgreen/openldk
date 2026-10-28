// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.nio.file.*;
import java.util.*;
public class SealedProbe {
    static sealed class Parent permits Child, OpenChild { static { initialized = true; } }
    static final class Child extends Parent {}
    static non-sealed class OpenChild extends Parent {}
    static final class Grandchild extends OpenChild {}
    sealed interface Contract permits Implementation {}
    static final class Implementation implements Contract {}
    static boolean initialized;
    static void check(boolean ok, String message) { if (!ok) throw new AssertionError(message); }
    static Set<Class<?>> permitted(Class<?> parent) { return Set.of(parent.getPermittedSubclasses()); }
    static class Loader extends ClassLoader {
        Loader() { super(SealedProbe.class.getClassLoader()); }
        Class<?> define(byte[] bytes) { return defineClass(null, bytes, 0, bytes.length); }
    }
    public static void main(String[] args) throws Exception {
        check(Parent.class.isSealed() && Contract.class.isSealed(), "sealed status");
        check(permitted(Parent.class).equals(Set.of(Child.class, OpenChild.class)), "class permits");
        check(permitted(Contract.class).equals(Set.of(Implementation.class)), "interface permits");
        check(!initialized, "reflection must not initialize classes");
        check(!OpenChild.class.isSealed() && OpenChild.class.getPermittedSubclasses() == null, "non-sealed child");
        check(!String.class.isSealed() && String.class.getPermittedSubclasses() == null, "ordinary class");
        check(!int.class.isSealed() && int.class.getPermittedSubclasses() == null, "primitive");
        check(!Parent[].class.isSealed() && Parent[].class.getPermittedSubclasses() == null, "array");
        Class<?> filtered = Class.forName("FilterRoot");
        check(filtered.isSealed() && filtered.getPermittedSubclasses().length == 1
                && filtered.getPermittedSubclasses()[0].getName().equals("PresentChild"), "missing/non-direct permitted types");
        Class<?>[] snapshot = Parent.class.getPermittedSubclasses();
        snapshot[0] = null;
        check(permitted(Parent.class).equals(Set.of(Child.class, OpenChild.class)), "defensive permitted array");
        check(new Grandchild() instanceof Parent && initialized, "non-sealed descendant");
        byte[] rogue = Files.readAllBytes(Path.of(args[0]));
        Loader loader = new Loader();
        for (int i = 0; i < 2; i++) {
            try { loader.define(rogue); throw new AssertionError("unpermitted subclass accepted"); }
            catch (IncompatibleClassChangeError expected) { }
        }
        byte[] rogueInterface = Files.readAllBytes(Path.of(args[1]));
        try { new Loader().define(rogueInterface); throw new AssertionError("unpermitted interface implementation accepted"); }
        catch (IncompatibleClassChangeError expected) { }
        try { new Loader().define(Files.readAllBytes(Path.of(args[3]))); throw new AssertionError("final sealed class accepted"); }
        catch (ClassFormatError expected) { }
        byte[] sameName = Files.readAllBytes(Path.of(args[2]));
        try { new Loader().define(sameName); throw new AssertionError("permitted name in wrong loader accepted"); }
        catch (IncompatibleClassChangeError expected) { }
        System.out.println("Sealed types: reflection, initialization, descendants, inheritance rejection, and loader identity passed");
    }
}
