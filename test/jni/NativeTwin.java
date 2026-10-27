// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
public class NativeTwin {
    public static void load(String path) { System.load(path); }
    public static native int value();
}
