// Copyright (C) 2026 Anthony Green <green@moxielogic.com>
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.io.*;
import java.lang.foreign.*;
import java.nio.*;
import java.nio.channels.*;
import java.nio.file.*;

public class MappedMemoryProbe {
    interface Operation { void run() throws Exception; }
    static void check(boolean value, String message) {
        if (!value) throw new AssertionError(message);
    }
    static void fails(Class<? extends Throwable> expected, Operation operation) throws Exception {
        try { operation.run(); throw new AssertionError("missing " + expected.getName()); }
        catch (Throwable error) { if (!expected.isInstance(error)) throw new AssertionError("wrong exception", error); }
    }
    static void bulkFloatCopies(MemorySegment segment) {
        float[] values = {-9f, 1f, -0f, Float.intBitsToFloat(0x7fc12345), -8f};
        for (ByteOrder order : new ByteOrder[]{ByteOrder.LITTLE_ENDIAN, ByteOrder.BIG_ENDIAN}) {
            ValueLayout.OfFloat layout = ValueLayout.JAVA_FLOAT_UNALIGNED.withOrder(order);
            segment.fill((byte)0x55);
            MemorySegment.copy(values, 1, segment, layout, 3, 3);
            float[] restored = {42f, 42f, 42f, 42f, 42f};
            MemorySegment.copy(segment, layout, 3, restored, 1, 3);
            check(restored[0] == 42f && restored[4] == 42f, "bulk copy preserves sentinels");
            for (int i = 1; i <= 3; i++) {
                int bits = Float.floatToRawIntBits(values[i]);
                check(Float.floatToRawIntBits(restored[i]) == bits, "bulk float copy bits");
                check(Float.floatToRawIntBits(segment.get(layout, 3 + (i-1)*4)) == bits,
                      "bulk copy agrees with scalar read");
            }
        }
    }
    public static void main(String[] args) throws Exception {
        Path path = Files.createTempFile("openldk-mapped", ".dat");
        try {
            int length = 131123;
            byte[] initial = new byte[length + 16];
            for (int i = 0; i < initial.length; i++) initial[i] = (byte)(i * 31);
            Files.write(path, initial);
            MappedByteBuffer mapped;
            try (FileChannel channel = FileChannel.open(path, StandardOpenOption.READ, StandardOpenOption.WRITE)) {
                mapped = channel.map(FileChannel.MapMode.READ_WRITE, 3, length);
                check(mapped.load() == mapped && mapped.isLoaded(), "load identity and residency");
                mapped.put(0, (byte)41); mapped.put(65537, (byte)42); mapped.put(length-1, (byte)43);
                mapped.position(17); check(mapped.force() == mapped && mapped.position() == 17, "force identity and position");
                check(mapped.force(65530, 41) == mapped, "unaligned partial force");
                mapped.force(0, 0); mapped.force(length, 0);
                MappedByteBuffer view = mapped.duplicate(); view.position(4091).limit(8197);
                MappedByteBuffer slice = view.slice();
                slice.put(10, (byte)44); slice.force(7, 19);
                check(slice.load() == slice && slice.isLoaded(), "unaligned slice load and residency");
                MappedByteBuffer readOnly = channel.map(FileChannel.MapMode.READ_ONLY, 3, length);
                check(readOnly.load().isLoaded() && readOnly.get(0) == 41, "read-only load");
                readOnly.force(1, 17);
                MappedByteBuffer copy = channel.map(FileChannel.MapMode.PRIVATE, 3, length);
                copy.put(0, (byte)99); copy.force();
                check(copy.get(0) == 99 && mapped.get(0) == 41, "private mapping isolation");
                MappedByteBuffer empty = channel.map(FileChannel.MapMode.READ_ONLY, 0, 0);
                check(empty.isLoaded() && empty.load() == empty && empty.force() == empty, "empty mapping");
            }
            // The native flush operation must work after the channel closes.
            mapped.put(1, (byte)45); mapped.force(0, 2); mapped.load();
            fails(IndexOutOfBoundsException.class, () -> mapped.force(-1, 1));
            fails(IndexOutOfBoundsException.class, () -> mapped.force(length, 1));
            byte[] restored = Files.readAllBytes(path);
            check(restored[3] == 41 && restored[4] == 45 && restored[3+65537] == 42
                  && restored[3+length-1] == 43 && restored[3+4091+10] == 44, "mapped writes persisted");
            MemorySegment segment;
            try (Arena arena = Arena.ofConfined()) {
                try (FileChannel channel = FileChannel.open(path, StandardOpenOption.READ, StandardOpenOption.WRITE)) {
                    segment = channel.map(FileChannel.MapMode.READ_WRITE, 1, 8193, arena);
                    check(segment.isMapped(), "FFM mapping kind");
                    segment.set(ValueLayout.JAVA_BYTE, 0, (byte)51); segment.force();
                    segment.load(); check(segment.isLoaded(), "FFM mapped residency");
                    segment.unload(); segment.load();
                    check(segment.get(ValueLayout.JAVA_BYTE, 0) == 51, "reload preserves flushed mapping");
                }
                segment.set(ValueLayout.JAVA_BYTE, 1, (byte)52); segment.force();
                MemorySegment slice = segment.asSlice(4093, 17); slice.load(); slice.force(); slice.unload();
            }
            fails(IllegalStateException.class, () -> segment.load());
            restored = Files.readAllBytes(path);
            check(restored[1] == 51 && restored[2] == 52, "FFM mapped writes persisted");
            bulkFloatCopies(MemorySegment.ofArray(new byte[64]));
            try (Arena arena = Arena.ofConfined()) {
                bulkFloatCopies(arena.allocate(64));
            }
            MemorySegment sharedSegment;
            try (Arena arena = Arena.ofShared();
                 FileChannel channel = FileChannel.open(path, StandardOpenOption.READ, StandardOpenOption.WRITE)) {
                sharedSegment = channel.map(FileChannel.MapMode.READ_WRITE, 1, 64, arena);
                bulkFloatCopies(sharedSegment);
            }
            MemorySegment closed = sharedSegment;
            fails(IllegalStateException.class, () -> closed.get(ValueLayout.JAVA_BYTE, 0));
            fails(IllegalStateException.class, () -> MemorySegment.copy(
                closed, ValueLayout.JAVA_FLOAT_UNALIGNED, 0, new float[1], 0, 1));
            System.out.println("Mapped buffers and FFM segments: load, residency, unload, flush, slices, modes, and lifetimes passed");
        } finally { Files.deleteIfExists(path); }
    }
}
