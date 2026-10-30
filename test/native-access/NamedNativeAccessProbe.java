// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.lang.module.*;
import java.lang.reflect.*;
import java.nio.file.*;
import java.util.*;

public class NamedNativeAccessProbe {
    static Object invoke(Class<?> type, String name) throws Throwable {
        try { return type.getMethod(name).invoke(null); }
        catch (InvocationTargetException e) { throw e.getCause(); }
    }
    public static void main(String[] args) throws Throwable {
        ModuleFinder finder = ModuleFinder.of(Path.of(args[0]));
        Configuration configuration = ModuleLayer.boot().configuration().resolve(finder, ModuleFinder.of(), Set.of("policy.owner", "policy.client"));
        ModuleLayer.Controller controller = ModuleLayer.defineModulesWithOneLoader(configuration, List.of(ModuleLayer.boot()), ClassLoader.getSystemClassLoader());
        ModuleLayer layer = controller.layer();
        Module owner = layer.findModule("policy.owner").orElseThrow();
        Module client = layer.findModule("policy.client").orElseThrow();
        controller.enableNativeAccess(owner);
        if (args.length > 2) controller.enableNativeAccess(client);
        Class<?> loader = layer.findLoader("policy.owner").loadClass("policyowner.Loader");
        Class<?> target = layer.findLoader("policy.client").loadClass("policyclient.Target");
        loader.getMethod("load", String.class).invoke(null, args[1]);
        System.out.println("owner=" + owner.isNativeAccessEnabled() + " client=" + client.isNativeAccessEnabled());
        // Native code is allowed to register arbitrary JNI bindings. This
        // path must remain callable even when automatic linking is denied.
        if (!invoke(target, "registered").equals(27)) throw new AssertionError();
        System.out.println("registered client=" + client.isNativeAccessEnabled());
        for (String method : new String[]{"symbol", "symbol", "segment"}) {
            try { System.out.println(method + "=" + invoke(target, method)); }
            catch (IllegalCallerException e) { System.out.println(method + "=IllegalCallerException"); }
        }
        System.out.println("after=" + client.isNativeAccessEnabled());
        System.out.println("Native access policy passed");
    }
}
