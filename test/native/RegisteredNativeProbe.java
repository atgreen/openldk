// Copyright (C) 2026 Anthony Green <green@moxielogic.com>
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
public class RegisteredNativeProbe {
    private int bias = 5;
    public static native int staticValue(int value);
    public native int instanceValue(int value);
    private native int privateValue(int value);
    public static int bytecodeValue(int value) { return -1000; }
    public static void main(String[] args) {
        RegisteredNativeProbe receiver = new RegisteredNativeProbe();
        int a = staticValue(5), b = receiver.instanceValue(3);
        int c = receiver.privateValue(2), d = bytecodeValue(4);
        if (a != 16 || b != 8 || c != 7 || d != 103)
            throw new AssertionError("Registered CL native results: " + a + "," + b + "," + c + "," + d);
        System.out.println("Registered CL static, instance, private, and bytecode bodies passed");
    }
}
