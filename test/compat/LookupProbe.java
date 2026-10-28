// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.lang.invoke.*;

public class LookupProbe {
    private static String ownPrivate() { return "own"; }
    static void check(boolean value, String label) { if (!value) throw new AssertionError(label); }
    public static void main(String[] args) throws Throwable {
        var lookup = MethodHandles.lookup();
        check(lookup.lookupClass() == LookupProbe.class, "caller identity");
        check(lookup.lookupModes() == 95, "ordinary lookup modes");
        check(lookup.hasFullPrivilegeAccess(), "own full privilege");
        var publicLookup = MethodHandles.publicLookup();
        check(!publicLookup.hasFullPrivilegeAccess(), "public lookup privilege");
        var own = lookup.findStatic(LookupProbe.class, "ownPrivate", MethodType.methodType(String.class));
        check(own.invokeWithArguments().equals("own"), "own private method");
        try {
            lookup.dropLookupMode(MethodHandles.Lookup.PRIVATE).findStatic(
                    LookupProbe.class, "ownPrivate", MethodType.methodType(String.class));
            throw new AssertionError("dropped private permission ignored");
        } catch (IllegalAccessException expected) { }
        try {
            publicLookup.findStatic(LookupProbe.class, "ownPrivate", MethodType.methodType(String.class));
            throw new AssertionError("public lookup found private method");
        } catch (IllegalAccessException expected) { }
        Class<?> unsafe = Class.forName("jdk.internal.misc.Unsafe");
        for (var candidate : new MethodHandles.Lookup[]{lookup, publicLookup}) {
            try {
                candidate.findStatic(unsafe, "getUnsafe", MethodType.methodType(unsafe));
                throw new AssertionError("concealed package accessible");
            } catch (IllegalAccessException expected) { }
        }
        try { MethodHandles.privateLookupIn(String.class, lookup); throw new AssertionError("closed package opened"); }
        catch (IllegalAccessException expected) { }
        System.out.println("Method handles: lookup modes, caller identity, private members, and module access checks passed");
    }
}
