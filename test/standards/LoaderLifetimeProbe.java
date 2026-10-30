// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.lang.classfile.*;
import java.lang.constant.*;
import java.lang.invoke.*;
import java.lang.ref.*;
import static java.lang.classfile.ClassFile.*;
import static java.lang.constant.ConstantDescs.*;
public class LoaderLifetimeProbe {
    static class Loader extends ClassLoader {
        Class<?> define(byte[] bytes) { return defineClass(null, bytes, 0, bytes.length); }
    }
    static void check(boolean value) { if (!value) throw new AssertionError(); }
    public static void main(String[] args) throws Throwable {
        byte[] bytes = ClassFile.of().build(ClassDesc.of("LifetimePayload"), b -> {
            b.withFlags(ACC_PUBLIC | ACC_SUPER);
            b.withMethodBody("value", MethodTypeDesc.of(CD_int), ACC_PUBLIC | ACC_STATIC,
                             c -> c.loadConstant(42).ireturn());
        });
        Loader loader = new Loader();
        var weak = new WeakReference<>(loader);
        Class<?> type = loader.define(bytes);
        var method = MethodHandles.publicLookup().findStatic(type, "value", MethodType.methodType(int.class));
        loader = null;
        for (int i = 0; i < 5; i++) System.gc();
        check(weak.get() != null && type.getClassLoader() == weak.get());
        check((int) method.invokeExact() == 42);
        check(new Loader().define(bytes) != type);
        Reference.reachabilityFence(type);
        Reference.reachabilityFence(method);
        System.out.println("Loader lifetime: live Class/MethodHandle roots and defining-loader isolation passed");
    }
}
