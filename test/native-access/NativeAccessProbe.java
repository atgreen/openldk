// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.lang.foreign.*;

public class NativeAccessProbe {
    static native int ping();
    public static void main(String[] args) {
        Module module = NativeAccessProbe.class.getModule();
        System.out.println("before=" + module.isNativeAccessEnabled());
        for (int i = 0; i < 2; i++) {
            try {
                switch (args[0]) {
                    case "jni", "mixed" -> { System.load(args[1]); if (ping() != 25) throw new AssertionError();
                        if (args[0].equals("mixed") && MemorySegment.ofAddress(1).reinterpret(8).byteSize() != 8) throw new AssertionError(); }
                    case "binding" -> ping();
                    case "boot" -> System.out.println("java.logging=" + ModuleLayer.boot().findModule("java.logging").orElseThrow().isNativeAccessEnabled());
                    case "null" -> System.load(null);
                    case "relative" -> System.load("relative.so");
                    case "missing" -> System.load(args[1] + ".missing");
                    case "path" -> System.loadLibrary("a/b");
                    case "runtime" -> Runtime.getRuntime().load(args[1]);
                    case "ffm" -> { if (MemorySegment.ofAddress(1).reinterpret(8).byteSize() != 8) throw new AssertionError(); }
                    default -> throw new AssertionError(args[0]);
                }
                System.out.println("allowed");
            } catch (IllegalCallerException | UnsatisfiedLinkError | NullPointerException e) {
                System.out.println(e.getClass().getSimpleName());
            }
        }
        System.out.println("after=" + module.isNativeAccessEnabled());
        System.out.println("Native access policy passed");
    }
}
