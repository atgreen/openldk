// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.lang.classfile.*;
import java.lang.classfile.attribute.*;
import java.lang.classfile.instruction.*;
import java.lang.constant.*;
import java.lang.invoke.*;
import java.util.*;
import static java.lang.classfile.ClassFile.*;
import static java.lang.constant.ConstantDescs.*;

public class ClassFileProbe {
    static int resolutions;
    static int failures;
    public static Object bootstrap(MethodHandles.Lookup lookup, String name, Class<?> type) {
        if (name.equals("failure")) { failures++; throw new IllegalArgumentException("bootstrap failed"); }
        if (name.equals("caller")) return lookup.lookupClass();
        if (name.equals("typed")) {
            if (type != lookup.lookupClass()) throw new AssertionError("wrong defining loader");
            return null;
        }
        if (name.equals("race")) {
            try { Thread.sleep(2); } catch (InterruptedException e) { throw new AssertionError(e); }
            return new Object();
        }
        if (name.equals("wide")) return Long.valueOf(0x123456789abcdefL);
        resolutions++; return new Object();
    }
    static int checks;
    static void check(boolean value) { checks++; if (!value) throw new AssertionError("check " + checks); }
    static void equal(Object actual, Object expected) {
        if (!Objects.equals(actual, expected)) throw new AssertionError(actual + " != " + expected);
    }
    static class Loader extends ClassLoader {
        Class<?> load(byte[] bytes) { return defineClass(null, bytes, 0, bytes.length); }
    }
    static Object invoke(Class<?> type, String name, Class<?> parameter, Object value) throws Exception {
        return type.getMethod(name, parameter).invoke(null, value);
    }
    public static void main(String[] args) throws Exception {
        ClassFile cf = ClassFile.of();
        ClassDesc generated = ClassDesc.of("GeneratedModern");
        var bsm = MethodHandleDesc.ofMethod(DirectMethodHandleDesc.Kind.STATIC,
                ClassDesc.of("ClassFileProbe"), "bootstrap", MethodTypeDesc.of(CD_Object,
                        ClassDesc.of("java.lang.invoke.MethodHandles$Lookup"), CD_String, CD_Class));
        var dynamic = DynamicConstantDesc.ofNamed(bsm, "cached", CD_Object);
        byte[] bytes = cf.build(generated, builder -> {
            builder.withFlags(ACC_PUBLIC | ACC_SUPER);
            builder.withMethodBody("choose", MethodTypeDesc.of(CD_int, CD_int), ACC_PUBLIC | ACC_STATIC,
                    code -> code.iload(0).ifThenElse(Opcode.IFLT,
                            negative -> negative.loadConstant(-7).ireturn(),
                            positive -> positive.iload(0).loadConstant(2).imul().ireturn()));
            builder.withMethodBody("divide", MethodTypeDesc.of(CD_int, CD_int), ACC_PUBLIC | ACC_STATIC,
                    code -> code.trying(body -> body.loadConstant(100).iload(0).idiv().ireturn(),
                            catches -> catches.catching(ClassDesc.of("java.lang.ArithmeticException"),
                                    handler -> handler.pop().loadConstant(-1).ireturn())));
            builder.withMethodBody("constant", MethodTypeDesc.of(CD_Object), ACC_PUBLIC | ACC_STATIC,
                    code -> code.ldc(dynamic).areturn());
            builder.withMethodBody("sameConstant", MethodTypeDesc.of(CD_Object), ACC_PUBLIC | ACC_STATIC,
                    code -> code.ldc(dynamic).areturn());
            builder.withMethodBody("nullConstant", MethodTypeDesc.of(CD_String), ACC_PUBLIC | ACC_STATIC,
                    code -> code.ldc(DynamicConstantDesc.ofNamed(BSM_NULL_CONSTANT, "nil", CD_String)).areturn());
        });
        // Add new methods through a transform as well as rewriting existing code below.
        bytes = cf.transformClass(cf.parse(bytes), ClassTransform.endHandler(builder -> {
            builder.withMethodBody("caller", MethodTypeDesc.of(CD_Class), ACC_PUBLIC | ACC_STATIC,
                    code -> code.ldc(DynamicConstantDesc.ofNamed(bsm, "caller", CD_Class)).areturn());
            builder.withMethodBody("wide", MethodTypeDesc.of(CD_long), ACC_PUBLIC | ACC_STATIC,
                    code -> code.ldc(DynamicConstantDesc.ofNamed(bsm, "wide", CD_long)).lreturn());
            builder.withMethodBody("typed", MethodTypeDesc.of(generated), ACC_PUBLIC | ACC_STATIC,
                    code -> code.ldc(DynamicConstantDesc.ofNamed(bsm, "typed", generated)).areturn());
            builder.withMethodBody("race", MethodTypeDesc.of(CD_Object), ACC_PUBLIC | ACC_STATIC,
                    code -> code.ldc(DynamicConstantDesc.ofNamed(bsm, "race", CD_Object)).areturn());
            for (String name : List.of("failure", "sameFailure"))
                builder.withMethodBody(name, MethodTypeDesc.of(CD_Object), ACC_PUBLIC | ACC_STATIC,
                        code -> code.ldc(DynamicConstantDesc.ofNamed(bsm, "failure", CD_Object)).areturn());
        }));
        ClassModel model = cf.parse(bytes);
        equal(model.thisClass().asSymbol(), generated); check(model.methods().size() == 11);
        check(cf.verify(bytes).isEmpty());
        check(model.methods().stream().filter(m -> m.methodName().equalsString("choose"))
                .flatMap(m -> m.code().stream()).anyMatch(code -> code.findAttribute(Attributes.stackMapTable()).isPresent()));
        Class<?> original = new Loader().load(bytes);
        equal(invoke(original, "choose", int.class, -1), -7);
        equal(invoke(original, "choose", int.class, 9), 18);
        equal(invoke(original, "divide", int.class, 0), -1);
        equal(invoke(original, "divide", int.class, 4), 25);
        Object constant = original.getMethod("constant").invoke(null);
        check(original.getMethod("constant").invoke(null) == constant);
        check(original.getMethod("sameConstant").invoke(null) == constant);
        check(resolutions == 1);
        check(original.getMethod("nullConstant").invoke(null) == null);
        check(original.getMethod("caller").invoke(null) == original);
        check(original.getMethod("typed").invoke(null) == null);
        equal(original.getMethod("wide").invoke(null), 0x123456789abcdefL);
        for (String name : List.of("failure", "failure", "sameFailure")) {
            try { original.getMethod(name).invoke(null); throw new AssertionError("bootstrap failure lost"); }
            catch (java.lang.reflect.InvocationTargetException expected) {
                check(expected.getCause() instanceof BootstrapMethodError);
                check(expected.getCause().getCause() instanceof IllegalArgumentException);
            }
        }
        check(failures == 1);
        // Rewrite a literal while retaining branches, exception tables, and frames.
        var transform = ClassTransform.transformingMethodBodies((builder, element) -> {
            if (element instanceof ConstantInstruction instruction && Integer.valueOf(2).equals(instruction.constantValue()))
                builder.loadConstant(3);
            else builder.with(element);
        });
        byte[] transformed = cf.transformClass(model, transform);
        check(cf.verify(transformed).isEmpty());
        Class<?> changed = new Loader().load(transformed);
        equal(invoke(changed, "choose", int.class, 9), 27);
        equal(invoke(changed, "choose", int.class, -1), -7);
        equal(invoke(changed, "divide", int.class, 0), -1);
        Object other = changed.getMethod("constant").invoke(null);
        check(other != constant && changed.getMethod("sameConstant").invoke(null) == other);
        check(resolutions == 2);
        check(changed.getMethod("caller").invoke(null) == changed);
        check(changed.getMethod("typed").invoke(null) == null);
        // The same bytes in a third defining loader must have a distinct resolution.
        Class<?> third = new Loader().load(bytes);
        check(third.getMethod("constant").invoke(null) != constant && resolutions == 3);
        var start = new java.util.concurrent.CountDownLatch(1);
        var failure = new java.util.concurrent.atomic.AtomicReference<Throwable>();
        Object[] results = new Object[12]; Thread[] workers = new Thread[results.length];
        var method = third.getMethod("race");
        for (int i = 0; i < workers.length; i++) {
            final int id = i;
            workers[i] = new Thread(() -> {
                try { start.await(); results[id] = method.invoke(null); }
                catch (Throwable e) { failure.compareAndSet(null, e); }
            });
            workers[i].start();
        }
        start.countDown();
        for (Thread worker : workers) { worker.join(15000); check(!worker.isAlive()); }
        if (failure.get() != null) throw new AssertionError(failure.get());
        for (Object result : results) check(result != null && result == results[0]);
        System.out.println("Class-File API: generation, branches, stack maps, handlers, transformation, and loader-local dynamic constants passed");
    }
}
