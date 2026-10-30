// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.lang.classfile.*;
import java.lang.constant.*;
import java.lang.reflect.*;
import java.util.*;
import java.util.concurrent.atomic.*;
import static java.lang.classfile.ClassFile.*;
import static java.lang.constant.ConstantDescs.*;

public class LoaderConstraintProbe {
    static final ClassDesc TOKEN = ClassDesc.of("lc.Token"), API = ClassDesc.of("lc.Api"),
        CLIENT = ClassDesc.of("lc.Client"), CHILD = ClassDesc.of("lc.Child"), FACE = ClassDesc.of("lc.Face");
    static final ClassFile CF = ClassFile.of();
    static int scenarios;
    static class Loader extends ClassLoader {
        final Map<String, byte[]> definitions = new HashMap<>();
        final Map<String, Class<?>> imports = new HashMap<>();
        int loads;
        boolean failApi;
        Loader() { super(LoaderConstraintProbe.class.getClassLoader()); definitions.put("lc.Token", token()); }
        @Override protected Class<?> loadClass(String name, boolean resolve) throws ClassNotFoundException {
            synchronized (getClassLoadingLock(name)) {
                if (failApi && name.equals("lc.Api")) throw new LinkageError("deliberate loader failure");
                Class<?> type = findLoadedClass(name);
                if (type == null) {
                    loads++;
                    type = imports.get(name);
                    if (type == null && definitions.containsKey(name)) type = define(name);
                    if (type == null) type = super.loadClass(name, false);
                }
                if (resolve) resolveClass(type);
                return type;
            }
        }
        Class<?> define(String name) { byte[] bytes = definitions.get(name); return defineClass(name, bytes, 0, bytes.length); }
        Class<?> get(String name) throws Exception { return Class.forName(name, true, this); }
    }
    static byte[] token() { return CF.build(TOKEN, b -> b.withFlags(ACC_PUBLIC | ACC_SUPER)); }
    static void constructor(ClassBuilder b, ClassDesc owner, boolean typed) {
        b.withMethodBody("<init>", typed ? MethodTypeDesc.of(CD_void, TOKEN) : MethodTypeDesc.of(CD_void),
            ACC_PUBLIC, c -> c.aload(0).invokespecial(CD_Object, "<init>", MethodTypeDesc.of(CD_void)).return_());
    }
    static byte[] api(boolean face, ClassDesc type, int flags) {
        return CF.build(type, b -> {
            b.withFlags(ACC_PUBLIC | (face ? ACC_INTERFACE | ACC_ABSTRACT : ACC_SUPER));
            if (!face) {
                constructor(b, type, false); constructor(b, type, true);
                b.withField("field", TOKEN, ACC_PUBLIC | ACC_STATIC);
                b.withField("instance", TOKEN, ACC_PUBLIC);
                b.withMethodBody("call", MethodTypeDesc.of(CD_void, TOKEN), ACC_PUBLIC | ACC_STATIC, c -> c.return_());
                b.withMethodBody("array", MethodTypeDesc.of(TOKEN.arrayType(2), TOKEN.arrayType(2)),
                    ACC_PUBLIC | ACC_STATIC, c -> c.aload(0).areturn());
                b.withMethodBody("take", MethodTypeDesc.of(CD_void, TOKEN), flags, c -> c.return_());
            } else b.withMethod("take", MethodTypeDesc.of(CD_void, TOKEN), ACC_PUBLIC | ACC_ABSTRACT, m -> {});
        });
    }
    static byte[] client(String kind) {
        return CF.build(CLIENT, b -> {
            b.withFlags(ACC_PUBLIC | ACC_SUPER);
            for (String name : List.of("run", "again")) b.withMethodBody(name, MethodTypeDesc.of(CD_void), ACC_PUBLIC | ACC_STATIC, c -> {
                switch (kind) {
                    case "field" -> c.getstatic(API, "field", TOKEN).pop();
                    case "instance-field" -> c.new_(API).dup().invokespecial(API,"<init>",MethodTypeDesc.of(CD_void))
                        .getfield(API,"instance",TOKEN).pop();
                    case "static" -> c.aconst_null().invokestatic(API,"call",MethodTypeDesc.of(CD_void,TOKEN));
                    case "array" -> c.aconst_null().invokestatic(API,"array",MethodTypeDesc.of(TOKEN.arrayType(2),TOKEN.arrayType(2))).pop();
                    case "constructor" -> c.new_(API).dup().aconst_null().invokespecial(API,"<init>",MethodTypeDesc.of(CD_void,TOKEN)).pop();
                    case "virtual" -> c.new_(API).dup().invokespecial(API,"<init>",MethodTypeDesc.of(CD_void))
                        .aconst_null().invokevirtual(API,"take",MethodTypeDesc.of(CD_void,TOKEN));
                    case "interface" -> c.aconst_null().aconst_null().invokeinterface(FACE,"take",MethodTypeDesc.of(CD_void,TOKEN));
                    default -> throw new AssertionError(kind);
                }
                c.return_();
            });
        });
    }
    static void invoke(Class<?> client, String name) throws Throwable {
        try { client.getMethod(name).invoke(null); }
        catch (InvocationTargetException e) { throw e.getCause(); }
    }
    interface Action { void run() throws Throwable; }
    static LinkageError rejected(Action action) throws Throwable {
        try { action.run(); throw new AssertionError("missing loader constraint violation"); }
        catch (LinkageError expected) {
            if (expected instanceof NoClassDefFoundError || expected instanceof NoSuchMethodError
                || expected instanceof NoSuchFieldError || expected instanceof VerifyError)
                throw new AssertionError("wrong linkage error", expected);
            return expected;
        }
    }
    static void resolution(String kind, boolean conflict, boolean late) throws Throwable {
        scenarios++;
        Loader provider = new Loader(), caller = new Loader();
        boolean face = kind.equals("interface");
        provider.definitions.put(face ? "lc.Face" : "lc.Api", api(face, face ? FACE : API, ACC_PUBLIC));
        caller.imports.put(face ? "lc.Face" : "lc.Api", provider.get(face ? "lc.Face" : "lc.Api"));
        provider.get("lc.Token");
        if (!conflict) caller.imports.put("lc.Token", provider.get("lc.Token"));
        if (!late) caller.get("lc.Token");
        caller.definitions.put("lc.Client", client(kind));
        Class<?> client = caller.get("lc.Client");
        if (conflict && !late) {
            LinkageError first = rejected(() -> invoke(client, "run"));
            LinkageError second = rejected(() -> invoke(client, "again"));
            LinkageError third = rejected(() -> invoke(client, "run"));
            if (first.getClass() != second.getClass() || !Objects.equals(first.getMessage(), second.getMessage())
                || !Objects.equals(first.getMessage(), third.getMessage())) throw new AssertionError("unstable resolution failure");
        } else {
            // A null receiver is expected only after successful interface resolution.
            try { invoke(client, "run"); }
            catch (NullPointerException e) { if (!face) throw e; }
            if (late && conflict) {
                rejected(() -> caller.define("lc.Token"));
                caller.imports.put("lc.Token", provider.get("lc.Token"));
                caller.definitions.remove("lc.Token");
                if (caller.get("lc.Token") != provider.get("lc.Token")) throw new AssertionError("failed definition poisoned initiation");
            }
        }
    }
    static void override(int flags, boolean conflict) throws Throwable {
        scenarios++;
        Loader parent = new Loader(), child = new Loader();
        parent.definitions.put("lc.Api", api(false, API, flags));
        child.imports.put("lc.Api", parent.get("lc.Api"));
        parent.get("lc.Token"); child.get("lc.Token");
        child.definitions.put("lc.Child", CF.build(CHILD, b -> {
            b.withFlags(ACC_PUBLIC | ACC_SUPER); b.withSuperclass(API);
            b.withMethodBody("take", MethodTypeDesc.of(CD_void,TOKEN), ACC_PUBLIC, c -> c.return_());
        }));
        Action load = () -> { var c = child.get("lc.Child"); c.getDeclaredMethods(); };
        if (conflict) rejected(load); else load.run();
    }
    static void transitive(boolean conflict) throws Throwable {
        scenarios++;
        Loader a = new Loader(), b = new Loader(), c = new Loader();
        a.definitions.put("lc.Api", api(false, API, ACC_PUBLIC));
        Class<?> token = a.get("lc.Token");
        b.imports.put("lc.Api", a.get("lc.Api"));
        b.definitions.put("lc.Client", client("field"));
        invoke(b.get("lc.Client"), "run"); // Token^a = Token^b, with b's type unresolved.
        ClassDesc middle = ClassDesc.of("lc.Middle");
        b.definitions.put("lc.Middle", CF.build(middle, builder -> {
            builder.withFlags(ACC_PUBLIC | ACC_SUPER);
            builder.withField("field", TOKEN, ACC_PUBLIC | ACC_STATIC);
        }));
        c.imports.put("lc.Api", b.get("lc.Middle"));
        // The symbolic class name must match the imported class's actual name.
        c.definitions.put("lc.Client", CF.build(CLIENT, builder -> {
            builder.withFlags(ACC_PUBLIC | ACC_SUPER);
            builder.withMethodBody("run", MethodTypeDesc.of(CD_void), ACC_PUBLIC | ACC_STATIC,
                code -> code.getstatic(middle, "field", TOKEN).pop().return_());
        }));
        c.imports.put("lc.Middle", c.imports.remove("lc.Api"));
        if (conflict) c.get("lc.Token");
        Class<?> client = c.get("lc.Client");
        if (conflict) rejected(() -> invoke(client, "run"));
        else { invoke(client, "run"); rejected(() -> c.define("lc.Token")); }
        b.imports.put("lc.Token", token); b.definitions.remove("lc.Token");
        if (b.get("lc.Token") != token) throw new AssertionError("failed constraint merge was not retracted");
    }
    static void inheritedImplementation() throws Throwable {
        scenarios++;
        Loader base = new Loader(), face = new Loader(), child = new Loader();
        base.definitions.put("lc.Api", api(false, API, ACC_PUBLIC));
        face.definitions.put("lc.Face", api(true, FACE, ACC_PUBLIC));
        base.get("lc.Token"); face.get("lc.Token");
        child.imports.put("lc.Api", base.get("lc.Api")); child.imports.put("lc.Face", face.get("lc.Face"));
        child.definitions.put("lc.Child", CF.build(CHILD, b -> {
            b.withFlags(ACC_PUBLIC | ACC_SUPER); b.withSuperclass(API); b.withInterfaceSymbols(FACE);
        }));
        rejected(() -> { var type = child.get("lc.Child"); type.getDeclaredMethods(); });
    }
    static void inheritedMember(boolean method, boolean conflict) throws Throwable {
        scenarios++;
        Loader provider = new Loader(), symbolic = new Loader(), caller = new Loader();
        provider.definitions.put("lc.Api", api(false, API, ACC_PUBLIC));
        Class<?> token = provider.get("lc.Token"); symbolic.get("lc.Token");
        symbolic.imports.put("lc.Api", provider.get("lc.Api"));
        ClassDesc middle = ClassDesc.of("lc.Middle");
        symbolic.definitions.put("lc.Middle", CF.build(middle, b -> {
            b.withFlags(ACC_PUBLIC | ACC_SUPER); b.withSuperclass(API);
        }));
        caller.imports.put("lc.Middle", symbolic.get("lc.Middle"));
        if (!conflict) caller.imports.put("lc.Token", token);
        caller.get("lc.Token");
        caller.definitions.put("lc.Client", CF.build(CLIENT, b -> {
            b.withFlags(ACC_PUBLIC | ACC_SUPER);
            b.withMethodBody("run", MethodTypeDesc.of(CD_void), ACC_PUBLIC | ACC_STATIC, c -> {
                if (method) c.aconst_null().invokestatic(middle, "call", MethodTypeDesc.of(CD_void,TOKEN));
                else c.getstatic(middle, "field", TOKEN).pop();
                c.return_();
            });
        }));
        Class<?> client = caller.get("lc.Client");
        if (conflict) rejected(() -> invoke(client, "run")); else invoke(client, "run");
    }
    static void interfaceOverride() throws Throwable {
        scenarios++;
        Loader parent = new Loader(), child = new Loader();
        parent.definitions.put("lc.Face", api(true, FACE, ACC_PUBLIC));
        parent.get("lc.Token"); child.get("lc.Token");
        child.imports.put("lc.Face", parent.get("lc.Face"));
        child.definitions.put("lc.Child", CF.build(CHILD, b -> {
            b.withFlags(ACC_PUBLIC | ACC_INTERFACE | ACC_ABSTRACT); b.withInterfaceSymbols(FACE);
            b.withMethod("take", MethodTypeDesc.of(CD_void,TOKEN), ACC_PUBLIC | ACC_ABSTRACT, m -> {});
        }));
        child.definitions.put("lc.Impl", CF.build(ClassDesc.of("lc.Impl"), b -> {
            b.withFlags(ACC_PUBLIC | ACC_SUPER); b.withInterfaceSymbols(CHILD);
            b.withMethodBody("take", MethodTypeDesc.of(CD_void,TOKEN), ACC_PUBLIC, c -> c.return_());
        }));
        rejected(() -> { var type = child.get("lc.Impl"); type.getDeclaredMethods(); });
    }
    static void privateImplementation() throws Throwable {
        scenarios++;
        Loader provider = new Loader(), child = new Loader();
        provider.definitions.put("lc.Api", api(false, API, ACC_PUBLIC));
        provider.definitions.put("lc.Face", api(true, FACE, ACC_PUBLIC));
        provider.get("lc.Token"); child.get("lc.Token");
        child.imports.put("lc.Api", provider.get("lc.Api")); child.imports.put("lc.Face", provider.get("lc.Face"));
        child.definitions.put("lc.Child", CF.build(CHILD, b -> {
            b.withFlags(ACC_PUBLIC | ACC_SUPER); b.withSuperclass(API); b.withInterfaceSymbols(FACE);
            b.withMethodBody("take", MethodTypeDesc.of(CD_void,TOKEN), ACC_PRIVATE, c -> c.return_());
        }));
        child.get("lc.Child").getDeclaredMethods(); // Private lookalike does not implement Face.take.
    }
    static void racingConstraints() throws Throwable {
        scenarios++;
        Loader a = new Loader(), b = new Loader(), c = new Loader();
        ClassDesc left = ClassDesc.of("lc.Left"), right = ClassDesc.of("lc.Right");
        a.get("lc.Token"); c.get("lc.Token");
        for (int k = 0; k < 2; k++) {
            Loader source = k == 0 ? a : c;
            ClassDesc type = k == 0 ? left : right;
            String name = k == 0 ? "lc.Left" : "lc.Right";
            source.definitions.put(name, CF.build(type, builder -> {
                builder.withFlags(ACC_PUBLIC | ACC_SUPER);
                builder.withField("field", TOKEN, ACC_PUBLIC | ACC_STATIC);
            }));
            b.imports.put(name, source.get(name));
        }
        b.definitions.put("lc.Client", CF.build(CLIENT, builder -> {
            builder.withFlags(ACC_PUBLIC | ACC_SUPER);
            builder.withMethodBody("left", MethodTypeDesc.of(CD_void), ACC_PUBLIC | ACC_STATIC,
                code -> code.getstatic(left,"field",TOKEN).pop().return_());
            builder.withMethodBody("right", MethodTypeDesc.of(CD_void), ACC_PUBLIC | ACC_STATIC,
                code -> code.getstatic(right,"field",TOKEN).pop().return_());
        }));
        Class<?> client = b.get("lc.Client");
        var successes = new AtomicInteger(); var failures = new AtomicInteger();
        var error = new AtomicReference<Throwable>();
        Thread[] threads = new Thread[2];
        for (int k = 0; k < 2; k++) {
            String name = k == 0 ? "left" : "right";
            threads[k] = new Thread(() -> {
                try { invoke(client,name); successes.incrementAndGet(); }
                catch (LinkageError e) { failures.incrementAndGet(); }
                catch (Throwable e) { error.set(e); }
            });
        }
        threads[0].start(); threads[1].start(); threads[0].join(); threads[1].join();
        if (error.get() != null) throw new AssertionError(error.get());
        if (successes.get() != 1 || failures.get() != 1) throw new AssertionError("non-atomic constraint publication");
    }
    static void cachedLoaderFailure() throws Throwable {
        scenarios++;
        Loader provider = new Loader(), caller = new Loader();
        provider.definitions.put("lc.Api", api(false,API,ACC_PUBLIC));
        caller.imports.put("lc.Api",provider.get("lc.Api"));
        caller.definitions.put("lc.Client", client("field"));
        Class<?> type = caller.get("lc.Client");
        caller.failApi = true;
        LinkageError first = rejected(() -> invoke(type,"run"));
        caller.failApi = false;
        LinkageError second = rejected(() -> invoke(type,"again"));
        LinkageError third = rejected(() -> invoke(type,"run"));
        if (!Objects.equals(first.getMessage(),second.getMessage()) || !Objects.equals(first.getMessage(),third.getMessage()))
            throw new AssertionError("failed CP resolution recovered after loader policy changed");
    }
    public static void main(String[] args) throws Throwable {
        for (String kind : List.of("field","instance-field","static","array","constructor","virtual","interface")) {
            resolution(kind, true, false);
            resolution(kind, false, false);
            resolution(kind, true, true);
            System.out.println(kind + " constraints passed");
        }
        transitive(false); transitive(true); cachedLoaderFailure();
        inheritedImplementation(); interfaceOverride(); privateImplementation();
        for (int k = 0; k < 4; k++) racingConstraints();
        for (boolean method : new boolean[]{false,true}) {
            inheritedMember(method, true); inheritedMember(method, false);
        }
        override(ACC_PUBLIC, true);
        override(ACC_PROTECTED, true);
        override(ACC_PRIVATE, false);
        override(0, false); // Same package name, different defining loaders.
        System.out.println("Loader constraints: " + scenarios + " scenarios covering resolution, delayed definitions, overriding and repeated failures passed");
    }
}
