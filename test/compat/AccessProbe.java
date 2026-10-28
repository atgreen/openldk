// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.lang.module.*;
import java.lang.reflect.*;
import java.nio.file.*;
import java.util.*;

public class AccessProbe {
    interface Action { void run() throws Exception; }
    static void check(boolean value, String label) { if (!value) throw new AssertionError(label); }
    static void denied(Action action) throws Exception {
        try { action.run(); throw new AssertionError("access allowed"); }
        catch (IllegalAccessException expected) { }
    }
    static void checkClass(Class<?> target, boolean exported, boolean open) throws Exception {
        System.out.println("checking " + target.getName());
        var method = target.getMethod("visible");
        var field = target.getField("PUBLIC");
        var ctor = target.getConstructor();
        check(method.canAccess(null) == exported, "canAccess method");
        check(field.canAccess(null) == exported, "canAccess field");
        check(ctor.canAccess(null) == exported, "canAccess constructor");
        if (exported) {
            check(method.invoke(null).equals("public"), "public invoke");
            check(field.get(null).equals("public"), "public get");
            field.set(null, "updated"); check(method.invoke(null).equals("updated"), "public set");
            field.set(null, "public"); check(ctor.newInstance().getClass() == target, "public new");
        } else {
            denied(() -> method.invoke(null)); denied(() -> field.get(null));
            denied(() -> field.set(null, "bad")); denied(() -> ctor.newInstance());
        }
        var privateMethod = target.getDeclaredMethod("hidden");
        var privateField = target.getDeclaredField("SECRET");
        var privateCtor = target.getDeclaredConstructor(int.class);
        denied(() -> privateMethod.invoke(null)); denied(() -> privateField.get(null));
        denied(() -> privateField.set(null, "bad")); denied(() -> privateCtor.newInstance(1));
        for (AccessibleObject member : List.of(privateMethod, privateField, privateCtor)) {
            check(member.trySetAccessible() == open, "trySetAccessible " + member);
            if (!open) {
                try { member.setAccessible(true); throw new AssertionError("setAccessible allowed"); }
                catch (InaccessibleObjectException expected) { }
            }
        }
        if (open) {
            check(privateMethod.invoke(null).equals("secret"), "open method");
            check(privateField.get(null).equals("secret"), "open field");
            check(privateCtor.newInstance(1).getClass() == target, "open ctor");
            privateMethod.setAccessible(false); denied(() -> privateMethod.invoke(null));
        }
    }
    public static void main(String[] args) throws Exception {
        Module base = Object.class.getModule(); Module ours = AccessProbe.class.getModule();
        check(ours.canRead(base) && !base.canRead(ours), "unnamed readability");
        check(base.isExported("java.lang", ours) && !base.isOpen("java.lang", ours), "exports versus opens");
        var stringValue = String.class.getDeclaredField("value");
        check(!stringValue.trySetAccessible(), "JDK private field closed");
        denied(() -> stringValue.get("test"));
        Class<?> unsafe = Class.forName("jdk.internal.misc.Unsafe");
        denied(() -> unsafe.getMethod("getUnsafe").invoke(null));
        ModuleFinder finder = ModuleFinder.of(Path.of(args[0]));
        var config = ModuleLayer.boot().configuration().resolve(finder, ModuleFinder.of(), Set.of("access.fixture"));
        var controller = ModuleLayer.defineModulesWithOneLoader(config, List.of(ModuleLayer.boot()), ClassLoader.getSystemClassLoader());
        var layer = controller.layer(); var module = layer.findModule("access.fixture").orElseThrow();
        var loader = layer.findLoader("access.fixture");
        check(module.canRead(base) && !module.canRead(ours), "named readability");
        for (String pkg : List.of("api", "open", "hidden")) {
            Class<?> target = loader.loadClass("fixture." + pkg + ".Target");
            check(target.getModule() == module, "layer class ownership");
            check(java.lang.reflect.Array.newInstance(target, 0).getClass().getModule() == module, "array ownership");
            checkClass(target, !pkg.equals("hidden"), pkg.equals("open"));
        }
        Module logging = ModuleLayer.boot().findModule("java.logging").orElseThrow();
        check(!module.canRead(logging), "missing read edge");
        var readProbe = loader.loadClass("fixture.api.ReadProbe").getMethod("logging");
        try { readProbe.invoke(null); throw new AssertionError("unreadable module accessed"); }
        catch (InvocationTargetException expected) { check(expected.getCause() instanceof IllegalAccessError, "read failure kind"); }
        controller.addReads(module, logging);
        // A failed symbolic reference stays failed even after the graph changes.
        try { readProbe.invoke(null); throw new AssertionError("failed resolution was retried"); }
        catch (InvocationTargetException expected) { check(expected.getCause() instanceof IllegalAccessError, "cached read failure"); }
        check(loader.loadClass("fixture.api.ReadAfterProbe").getMethod("logging").invoke(null).equals("access"),
                "read edge permits a fresh bytecode reference");
        controller.addExports(module, "fixture.hidden", ours);
        checkClass(loader.loadClass("fixture.hidden.Target"), true, false);
        controller.addReads(module, ours); check(module.canRead(ours), "added read edge");
        controller.addOpens(module, "fixture.hidden", ours);
        checkClass(loader.loadClass("fixture.hidden.Target"), true, true);
        System.out.println("Module access: ownership, readability, exports, opens, and reflection checks passed");
    }
}
