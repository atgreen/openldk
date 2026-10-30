// Copyright (C) 2026 Anthony Green <green@moxielogic.com>
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.lang.foreign.*;
import java.nio.*;
import java.util.Arrays;

public class MismatchProbe {
    static void check(long actual, long expected, String message) {
        if (actual != expected) throw new AssertionError(message + ": " + actual + " expected " + expected);
    }
    public static void main(String[] args) {
        int[] ints = new int[40], ints2;
        long[] longs = new long[40], longs2;
        short[] shorts = new short[40], shorts2;
        char[] chars = new char[40], chars2;
        for (int i = 0; i < 40; i++) { ints[i] = i * 70001; longs[i] = 0x123400000000L + i;
            shorts[i] = (short)(i * 1700); chars[i] = (char)(i * 1700); }
        ints2 = ints.clone(); longs2 = longs.clone(); shorts2 = shorts.clone(); chars2 = chars.clone();
        check(Arrays.mismatch(ints, 1, 35, ints2, 1, 35), -1, "equal int range");
        check(Arrays.mismatch(longs, 1, 35, longs2, 1, 35), -1, "equal long range");
        check(Arrays.mismatch(shorts, 1, 35, shorts2, 1, 35), -1, "equal short range");
        check(Arrays.mismatch(chars, 1, 35, chars2, 1, 35), -1, "equal char range");
        ints2[19] ^= 1; longs2[19] ^= 1; shorts2[19] ^= 1; chars2[19] ^= 1;
        check(Arrays.mismatch(ints, 1, 35, ints2, 1, 35), 18, "int byte offset and scale");
        check(Arrays.mismatch(longs, 1, 35, longs2, 1, 35), 18, "long byte offset and scale");
        check(Arrays.mismatch(shorts, 1, 35, shorts2, 1, 35), 18, "short byte offset and scale");
        check(Arrays.mismatch(chars, 1, 35, chars2, 1, 35), 18, "char byte offset and scale");
        float[] floats = new float[20], floats2;
        double[] doubles = new double[20], doubles2;
        floats[9] = Float.intBitsToFloat(0x7fc00001); doubles[9] = Double.longBitsToDouble(0x7ff8000000000001L);
        floats2 = floats.clone(); doubles2 = doubles.clone();
        floats2[9] = Float.intBitsToFloat(0x7fc00002); doubles2[9] = Double.longBitsToDouble(0x7ff8000000000002L);
        check(Arrays.mismatch(floats, floats2), -1, "float NaN equivalence");
        check(Arrays.mismatch(doubles, doubles2), -1, "double NaN equivalence");
        floats2[12] = -0.0f; doubles2[12] = -0.0;
        check(Arrays.mismatch(floats, 1, 18, floats2, 1, 18), 11, "float signed zero");
        check(Arrays.mismatch(doubles, 1, 18, doubles2, 1, 18), 11, "double signed zero");
        int[] words = { 0x12345678, 0x23456789, 0x3456789a, 0x456789ab, 0x56789abc, 0x6789abcd };
        byte[] bytes = new byte[words.length * 4];
        ByteBuffer packed = ByteBuffer.wrap(bytes).order(ByteOrder.nativeOrder());
        for (int word : words) packed.putInt(word);
        MemorySegment array = MemorySegment.ofArray(bytes), other = MemorySegment.ofArray(words);
        check(array.mismatch(other), -1, "mixed primitive storage");
        check(array.asSlice(3, 17).mismatch(other.asSlice(3, 17)), -1, "unaligned primitive slices");
        bytes[10] ^= 1;
        check(array.asSlice(3, 17).mismatch(other.asSlice(3, 17)), 7, "mixed primitive byte index");
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment nativeA = arena.allocate(64, 1), nativeB = arena.allocate(64, 1);
            nativeA.asSlice(1, bytes.length).copyFrom(array); nativeB.copyFrom(nativeA);
            check(nativeA.asSlice(1, bytes.length).mismatch(array), -1, "native/array equality");
            check(nativeA.asSlice(1, bytes.length).mismatch(other), 10, "native/array mismatch");
            nativeB.set(ValueLayout.JAVA_BYTE, 41, (byte)77);
            check(nativeA.asSlice(0, 41).mismatch(nativeB.asSlice(0, 41)), -1, "do not compare outside native tail");
            nativeB.set(ValueLayout.JAVA_BYTE, 40, (byte)88);
            check(nativeA.asSlice(0, 41).mismatch(nativeB.asSlice(0, 41)), 40, "native final byte");
            nativeB.set(ValueLayout.JAVA_BYTE, 7, (byte)99);
            check(nativeA.asSlice(1, 40).mismatch(nativeB.asSlice(1, 40)), 6, "native unaligned index");
        }
        ByteBuffer heap = ByteBuffer.allocate(41), direct = ByteBuffer.allocateDirect(41);
        for (int i = 0; i < 41; i++) { heap.put(i, (byte)i); direct.put(i, (byte)i); }
        heap.position(1).limit(40); direct.position(1).limit(40);
        check(heap.mismatch(direct.asReadOnlyBuffer()), -1, "heap/direct buffer equality");
        direct.put(23, (byte)99);
        check(heap.mismatch(direct), 22, "heap/direct relative mismatch");
        check(Arrays.mismatch(new byte[0], new byte[0]), -1, "empty arrays");
        check(MemorySegment.ofArray(new byte[] {1,2,3}).mismatch(MemorySegment.ofArray(new byte[] {1,2})), 2, "prefix length");
        System.out.println("Primitive scales, NaNs, signed zero, heap/native buffers, slices, and mismatch tails passed");
    }
}
