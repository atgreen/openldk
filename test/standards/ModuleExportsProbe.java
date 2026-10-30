// Copyright (C) 2026 Anthony Green <green@moxielogic.com>
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.lang.reflect.Method;

public class ModuleExportsProbe {
    public static void main(String[] args) throws Exception {
        boolean expected = args.length != 0 && args[0].equals("exported");
        Class<?> action = Class.forName("jdk.internal.misc.VM");
        Module caller = ModuleExportsProbe.class.getModule();
        if (action.getModule().isExported("jdk.internal.misc", caller) != expected)
            throw new AssertionError("Incorrect qualified package export");
        Method method = action.getMethod("isBooted");
        boolean accessible;
        try {
            Object value = method.invoke(null);
            if (!(value instanceof Boolean)) throw new AssertionError("Wrong reflected result");
            accessible = true;
        } catch (IllegalAccessException denied) {
            accessible = false;
        }
        if (accessible != expected) throw new AssertionError("Incorrect reflection access");
        if (args.length > 1) {
            Module desktop = Class.forName("sun.awt.SunToolkit").getModule();
            if (!desktop.isExported("sun.awt", caller)) throw new AssertionError("Lost repeated export");
        }
        System.out.println("Module export checks passed: " + (accessible ? "EXPORTED" : "DENIED"));
    }
}
