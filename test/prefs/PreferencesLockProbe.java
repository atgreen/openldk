// Copyright (C) 2026 Anthony Green <green@moxielogic.com>
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0

import java.lang.reflect.Field;
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import sun.misc.Unsafe;

public class PreferencesLockProbe {
    static MethodHandle lock, unlock, chmod;
    static void check(boolean value) {
        if (!value) throw new AssertionError();
    }
    static int[] acquire(String path, boolean shared) throws Throwable {
        return (int[]) lock.invokeExact(path, 0600, shared);
    }
    public static void main(String[] args) throws Throwable {
        Class<?> type = Class.forName("java.util.prefs.FileSystemPreferences");
        // Test private native entry points without depending on launcher
        // support for --add-opens.  Public API behavior is tested separately.
        Field singleton = Unsafe.class.getDeclaredField("theUnsafe");
        singleton.setAccessible(true);
        Unsafe unsafe = (Unsafe) singleton.get(null);
        Field trusted = MethodHandles.Lookup.class.getDeclaredField("IMPL_LOOKUP");
        MethodHandles.Lookup lookup = (MethodHandles.Lookup) unsafe.getObject(
            unsafe.staticFieldBase(trusted), unsafe.staticFieldOffset(trusted));
        lock = lookup.findStatic(type, "lockFile0",
            MethodType.methodType(int[].class, String.class, int.class, boolean.class));
        unlock = lookup.findStatic(type, "unlockFile0", MethodType.methodType(int.class, int.class));
        chmod = lookup.findStatic(type, "chmod",
            MethodType.methodType(int.class, String.class, int.class));
        String mode = args[0], path = args[1];
        if (mode.equals("missing")) {
            int[] result = acquire(path, true);
            check(result[0] == 0 && result[1] == 2); // ENOENT
            check((int) chmod.invokeExact(path, 0600) == 2);
            check((int) unlock.invokeExact(-1) == 9); // EBADF
            System.out.println("PASS missing");
        } else if (mode.equals("chmod")) {
            check((int) chmod.invokeExact(path, 0640) == 0);
            System.out.println("PASS chmod");
        } else if (mode.equals("contended")) {
            for (int i = 0; i < 160; i++) {
                int[] result = acquire(path, false);
                check(result[0] == 0 && (result[1] == 11 || result[1] == 13));
            }
            System.out.println("PASS contended");
        } else {
            boolean shared = mode.equals("shared");
            int[] result = acquire(path, shared);
            check(result[0] > 0);
            System.out.println("LOCKED");
            System.out.flush();
            System.in.read();
            check((int) unlock.invokeExact(result[0]) == 0);
            System.out.println("RELEASED");
            System.out.flush();
            System.in.read();
        }
    }
}
