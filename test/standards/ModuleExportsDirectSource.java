// Copyright (C) 2026 Anthony Green <green@moxielogic.com>
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import jdk.internal.misc.VM;

class ModuleExportsDirectSource {
    public static void main(String[] args) {
        if (!VM.class.getModule().isExported("jdk.internal.misc", ModuleExportsDirectSource.class.getModule()))
            throw new AssertionError("Missing source launch export");
        VM.isBooted();
        System.out.println("Module export checks passed: EXPORTED");
    }
}
