// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
package payload;
public class UnloadingNative {
    public static Object self = new Object[] {UnloadingNative.class, UnloadingNative.class.getClassLoader()};
    public static void load(String path, boolean retain) { System.load(path); if (retain) pin(); }
    public static native int version();
    public static native void pin();
}
