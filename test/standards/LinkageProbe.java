// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.lang.classfile.*;
import java.lang.constant.*;
import static java.lang.classfile.ClassFile.*;
import static java.lang.constant.ConstantDescs.*;
public class LinkageProbe {
    public static class Parent { public final void done() {} }
    static class Loader extends ClassLoader {
        Class<?> define(byte[] bytes) { return defineClass(null, bytes, 0, bytes.length); }
    }
    static void rejected(byte[] bytes, Class<? extends LinkageError> expected) {
        try {
            var type = new Loader().define(bytes);
            type.getDeclaredMethods();
            throw new AssertionError("accepted invalid class " + type);
        } catch (LinkageError e) {
            if (!expected.isInstance(e)) throw new AssertionError(e);
        }
    }
    public static void main(String[] args) {
        var cf = ClassFile.of();
        rejected(cf.build(ClassDesc.of("FinalChild"), b -> {
            b.withFlags(ACC_PUBLIC | ACC_SUPER);
            b.withSuperclass(CD_String);
        }), IncompatibleClassChangeError.class);
        rejected(cf.build(ClassDesc.of("InterfaceSuper"), b -> {
            b.withFlags(ACC_PUBLIC | ACC_SUPER);
            b.withSuperclass(ClassDesc.of("java.lang.Runnable"));
        }), IncompatibleClassChangeError.class);
        rejected(cf.build(ClassDesc.of("ClassInterface"), b -> {
            b.withFlags(ACC_PUBLIC | ACC_SUPER);
            b.withInterfaceSymbols(CD_String);
        }), IncompatibleClassChangeError.class);
        rejected(cf.build(ClassDesc.of("FinalOverride"), b -> {
            b.withFlags(ACC_PUBLIC | ACC_SUPER);
            b.withSuperclass(ClassDesc.of("LinkageProbe$Parent"));
            b.withMethodBody("done", MethodTypeDesc.of(CD_void), ACC_PUBLIC, c -> c.return_());
        }), IncompatibleClassChangeError.class);
        System.out.println("Linkage: final superclass, final override and parent kinds passed");
    }
}
