// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.io.*;
import java.nio.file.*;

public class RemainderProbe {
    static long state = 0x123456789abcdefL;
    static long random() { state ^= state << 13; state ^= state >>> 7; state ^= state << 17; return state; }
    static float single(float x, float y) { return x % y; }
    static double twice(double x, double y) { return x % y; }
    static final int[] FLOATS = {0, 0x80000000, 1, 2, 3, 0x007fffff, 0x00800000, 0x00800001,
        0x3f000000, 0x3f800000, 0x40000000, 0x40e00000, 0x7f7fffff, 0xff7fffff,
        0xbf800000, 0xc0800000, 0x807fffff, 0x80800000, 0x7f800000, 0xff800000,
        0x7fc00000, 0x7f800001, 0xffc12345};
    static final long[] DOUBLES = {0L, Long.MIN_VALUE, 1L, 2L, 3L, 0xfffffffffffffL,
        0x10000000000000L, 0x10000000000001L, 0x3fe0000000000000L, 0x3ff0000000000000L,
        0x4000000000000000L, 0x401c000000000000L, 0x7fefffffffffffffL, 0xffefffffffffffffL,
        0xbff0000000000000L, 0xc010000000000000L, 0x800fffffffffffffL, 0x8010000000000000L,
        0x7ff0000000000000L, 0xfff0000000000000L, 0x7ff8000000000000L,
        0x7ff0000000000001L, 0xfff8123456789abcL};
    static void writeFloat(DataOutputStream out, int x, int y) throws IOException {
        out.writeInt(x); out.writeInt(y);
        out.writeInt(Float.floatToIntBits(single(Float.intBitsToFloat(x), Float.intBitsToFloat(y))));
    }
    static void writeDouble(DataOutputStream out, long x, long y) throws IOException {
        out.writeLong(x); out.writeLong(y);
        out.writeLong(Double.doubleToLongBits(twice(Double.longBitsToDouble(x), Double.longBitsToDouble(y))));
    }
    public static void main(String[] args) throws Exception {
        int randomCases = 100000;
        Path path = Path.of(args[1]);
        if (args[0].equals("generate")) {
            try (var out = new DataOutputStream(new BufferedOutputStream(Files.newOutputStream(path)))) {
                out.writeInt(FLOATS.length * FLOATS.length + randomCases);
                for (int x : FLOATS) for (int y : FLOATS) writeFloat(out, x, y);
                for (int i = 0; i < randomCases; i++) writeFloat(out, (int)random(), (int)random());
                out.writeInt(DOUBLES.length * DOUBLES.length + randomCases);
                for (long x : DOUBLES) for (long y : DOUBLES) writeDouble(out, x, y);
                for (int i = 0; i < randomCases; i++) writeDouble(out, random(), random());
            }
            return;
        }
        try (var in = new DataInputStream(new BufferedInputStream(Files.newInputStream(path)))) {
            int floats = in.readInt();
            for (int i = 0; i < floats; i++) {
                int x = in.readInt(), y = in.readInt(), expected = in.readInt();
                int actual = Float.floatToIntBits(single(Float.intBitsToFloat(x), Float.intBitsToFloat(y)));
                if (actual != expected) throw new AssertionError("float case " + i + ": " + Integer.toHexString(x)
                        + " % " + Integer.toHexString(y) + " = " + Integer.toHexString(actual) + " expected " + Integer.toHexString(expected));
            }
            int doubles = in.readInt();
            for (int i = 0; i < doubles; i++) {
                long x = in.readLong(), y = in.readLong(), expected = in.readLong();
                long actual = Double.doubleToLongBits(twice(Double.longBitsToDouble(x), Double.longBitsToDouble(y)));
                if (actual != expected) throw new AssertionError("double case " + i + ": " + Long.toHexString(x)
                        + " % " + Long.toHexString(y) + " = " + Long.toHexString(actual) + " expected " + Long.toHexString(expected));
            }
            if (in.read() != -1) throw new AssertionError("trailing input");
            System.out.println("Remainder: " + floats + " float and " + doubles + " double JDK vectors passed");
        }
    }
}
