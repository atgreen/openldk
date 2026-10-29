// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
package policyclient;
import java.lang.foreign.MemorySegment;
public class Target {
    public static native int symbol();
    public static native int registered();
    public static long segment() { return MemorySegment.ofAddress(1).reinterpret(8).byteSize(); }
}
