// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.lang.reflect.*;

public class ReflectionProbe {
    static final Exception problem = new Exception("checked target exception");
    public static void checked() throws Exception { throw problem; }
    public static void error() { throw new AssertionError("target error"); }
    public void instance() { }
    public static class ConstructorFailure {
        public ConstructorFailure() { throw new IllegalStateException("constructor body"); }
    }
    public static class BadStatic {
        static { if (System.nanoTime() != 0) throw new IllegalStateException("static initialization"); }
        public static void run() { }
    }
    public static class BadConstructor {
        static { if (System.nanoTime() != 0) throw new IllegalStateException("constructor initialization"); }
        public BadConstructor() { }
    }
    public static void main(String[] args) throws Exception {
        try { ReflectionProbe.class.getMethod("checked").invoke(null); throw new AssertionError(); }
        catch (InvocationTargetException expected) {
            if (expected.getCause() != problem) throw new AssertionError("Target identity lost");
        }
        try { ReflectionProbe.class.getMethod("error").invoke(null); throw new AssertionError(); }
        catch (InvocationTargetException expected) {
            if (!(expected.getCause() instanceof AssertionError)) throw expected;
        }
        try { ConstructorFailure.class.getConstructor().newInstance(); throw new AssertionError(); }
        catch (InvocationTargetException expected) {
            if (!(expected.getCause() instanceof IllegalStateException)) throw expected;
        }
        try { ReflectionProbe.class.getMethod("instance").invoke(null); throw new AssertionError(); }
        catch (NullPointerException expected) { }
        try { BadStatic.class.getMethod("run").invoke(null); throw new AssertionError(); }
        catch (ExceptionInInitializerError expected) {
            if (!(expected.getCause() instanceof IllegalStateException)) throw expected;
        }
        try { BadConstructor.class.getConstructor().newInstance(); throw new AssertionError(); }
        catch (ExceptionInInitializerError expected) {
            if (!(expected.getCause() instanceof IllegalStateException)) throw expected;
        }
        System.out.println("Reflection target wrapping, cause identity, and initialization boundaries passed");
    }
}
