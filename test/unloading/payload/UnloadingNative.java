// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
package payload;
public class UnloadingNative {
    public static Object self = new Object[] {UnloadingNative.class, UnloadingNative.class.getClassLoader()};
    public static void load(String path, boolean retain) {
        System.load(path);
        if (cachedVersion() != 25) throw new AssertionError("cached JNI member access");
        if (retain) pin();
    }
    public static native int version();
    public static native int cachedVersion();
    public static native void pin();
}
