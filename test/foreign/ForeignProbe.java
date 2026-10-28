// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.lang.foreign.*;
import java.lang.invoke.*;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.concurrent.atomic.AtomicReference;
import static java.lang.foreign.ValueLayout.*;
import static java.lang.foreign.MemoryLayout.PathElement.*;

public class ForeignProbe {
    static final Linker linker = Linker.nativeLinker();
    static int callbacks;
    static String stage = "setup";
    static int compare(MemorySegment a, MemorySegment b) {
        callbacks++;
        return Integer.compare(a.get(JAVA_INT, 0), b.get(JAVA_INT, 0));
    }
    static long sum(long a, long b) { return a + b; }
    static byte decrement(byte value) { return (byte)(value - 1); }
    static double sumFloating(float a, double b) { return a + b; }
    static void check(boolean value) { if (!value) throw new AssertionError(stage); }
    static MethodHandle function(SymbolLookup lookup, String name, FunctionDescriptor descriptor, Linker.Option... options) {
        return linker.downcallHandle(lookup.findOrThrow(name), descriptor, options);
    }
    public static void main(String[] args) throws Throwable {
        SymbolLookup libc = linker.defaultLookup();
        check(libc.find("__openldk_missing_symbol__").isEmpty());
        check(libc.find("strlen\0suffix").isEmpty());
        try { libc.find(null); throw new AssertionError(); } catch (NullPointerException expected) { }
        check(linker.canonicalLayouts().get("size_t").byteSize() == 8);
        MethodHandle strlen = function(libc, "strlen", FunctionDescriptor.of(JAVA_LONG, ADDRESS));
        MemorySegment dead;
        MemorySegment expiredComparator;
        MethodHandle staleLibrary;
        MethodHandle qsort;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment text = arena.allocateFrom("hello λ");
            dead = text;
            check((long) strlen.invokeExact(text) == 8L);
            MethodHandle unbound = linker.downcallHandle(FunctionDescriptor.of(JAVA_LONG, ADDRESS));
            check((long) unbound.invokeExact(libc.findOrThrow("strlen"), text) == 8L);
            AtomicReference<Throwable> wrongThread = new AtomicReference<>();
            Thread worker = new Thread(() -> {
                try { long ignored = (long) strlen.invokeExact(text); wrongThread.set(new AssertionError("Confined argument accepted: " + ignored)); }
                catch (WrongThreadException expected) { }
                catch (Throwable error) { wrongThread.set(error); }
            });
            worker.start(); worker.join();
            if (wrongThread.get() != null) throw new AssertionError(wrongThread.get());
            SymbolLookup lib = SymbolLookup.libraryLookup(Path.of(args[0]), arena);
            staleLibrary = function(lib, "ffi_add", FunctionDescriptor.of(JAVA_INT, JAVA_INT, JAVA_INT));
            check((int)function(lib, "ffi_add", FunctionDescriptor.of(JAVA_INT, JAVA_INT, JAVA_INT)).invokeExact(3, 4) == 7);
            check((long)function(lib, "ffi_long", FunctionDescriptor.of(JAVA_LONG, JAVA_LONG)).invokeExact(-1234567890123L)
                    == (-1234567890123L ^ 0x1234567812345678L));
            check((byte)function(lib, "ffi_byte", FunctionDescriptor.of(JAVA_BYTE, JAVA_BYTE)).invokeExact((byte)-100) == -101);
            check((short)function(lib, "ffi_short", FunctionDescriptor.of(JAVA_SHORT, JAVA_SHORT)).invokeExact((short)-20000) == -20002);
            check((char)function(lib, "ffi_char", FunctionDescriptor.of(JAVA_CHAR, JAVA_CHAR)).invokeExact((char)50000) == 50003);
            check(!(boolean)function(lib, "ffi_bool", FunctionDescriptor.of(JAVA_BOOLEAN, JAVA_BOOLEAN)).invokeExact(true));
            check((float)function(lib, "ffi_float", FunctionDescriptor.of(JAVA_FLOAT, JAVA_FLOAT)).invokeExact(2.5f) == 3.75f);
            check((double)function(lib, "ffi_double", FunctionDescriptor.of(JAVA_DOUBLE, JAVA_DOUBLE, JAVA_DOUBLE)).invokeExact(2.5d, -4d) == -10d);
            int[] heap = {0};
            MemorySegment heapSegment = MemorySegment.ofArray(heap);
            MethodHandle alias = function(lib, "ffi_alias", FunctionDescriptor.of(JAVA_INT, ADDRESS, ADDRESS), Linker.Option.critical(true));
            check((int)alias.invokeExact(heapSegment, heapSegment) == 42 && heap[0] == 41);
            try { long ignored = (long)strlen.invokeExact(heapSegment); throw new AssertionError("Heap argument accepted: " + ignored); }
            catch (IllegalArgumentException expected) { }
            stage = "struct return";
            StructLayout pair = MemoryLayout.structLayout(JAVA_INT, MemoryLayout.paddingLayout(4), JAVA_DOUBLE);
            MemorySegment p = arena.allocate(pair);
            p.set(JAVA_INT, 0, 12); p.set(JAVA_DOUBLE, 8, 3d);
            MethodHandle transform = function(lib, "ffi_pair", FunctionDescriptor.of(pair, pair, JAVA_DOUBLE));
            MemorySegment result = (MemorySegment)transform.invokeExact((SegmentAllocator)arena, p, 2.5d);
            check(result.get(JAVA_INT, 0) == 17 && result.get(JAVA_DOUBLE, 8) == 7.5d);
            stage = "errno capture";
            MemorySegment capture = arena.allocate(Linker.Option.captureStateLayout());
            MethodHandle errno = function(lib, "ffi_errno", FunctionDescriptor.of(JAVA_INT, JAVA_INT), Linker.Option.captureCallState("errno"));
            check((int)errno.invokeExact(capture, 22) == -1);
            check(capture.get(JAVA_INT, Linker.Option.captureStateLayout().byteOffset(groupElement("errno"))) == 22);
            stage = "variadic call";
            MethodHandle snprintf = function(libc, "snprintf", FunctionDescriptor.of(JAVA_INT, ADDRESS, JAVA_LONG, ADDRESS, JAVA_INT, JAVA_DOUBLE), Linker.Option.firstVariadicArg(3));
            MemorySegment output = arena.allocate(64);
            int written = (int)snprintf.invokeExact(output, 64L, arena.allocateFrom("%d %.1f"), 42, 2.5d);
            check(written == 6 && output.getString(0).equals("42 2.5"));
            stage = "qsort upcall";
            FunctionDescriptor comparatorType = FunctionDescriptor.of(JAVA_INT, ADDRESS.withTargetLayout(JAVA_INT), ADDRESS.withTargetLayout(JAVA_INT));
            MethodHandle comparator = MethodHandles.lookup().findStatic(ForeignProbe.class, "compare", comparatorType.toMethodType());
            MemorySegment comparatorStub = linker.upcallStub(comparator, comparatorType, arena);
            expiredComparator = comparatorStub;
            int[] values = {9, -3, 5, 0, 2};
            MemorySegment array = arena.allocateFrom(JAVA_INT, values);
            qsort = function(libc, "qsort", FunctionDescriptor.ofVoid(ADDRESS, JAVA_LONG, JAVA_LONG, ADDRESS));
            qsort.invokeExact(array, 5L, 4L, comparatorStub);
            check(Arrays.equals(array.toArray(JAVA_INT), new int[]{-3, 0, 2, 5, 9}) && callbacks > 0);
            stage = "native-thread upcall";
            FunctionDescriptor sumType = FunctionDescriptor.of(JAVA_LONG, JAVA_LONG, JAVA_LONG);
            MemorySegment sumStub = linker.upcallStub(MethodHandles.lookup().findStatic(ForeignProbe.class, "sum", sumType.toMethodType()), sumType, arena);
            MethodHandle threaded = function(lib, "ffi_thread_callback", FunctionDescriptor.of(JAVA_LONG, ADDRESS, JAVA_LONG));
            check((long)threaded.invokeExact(sumStub, 4_000_000_000L) == 4_000_000_007L);
            stage = "narrow and floating upcalls";
            FunctionDescriptor byteType = FunctionDescriptor.of(JAVA_BYTE, JAVA_BYTE);
            MemorySegment byteStub = linker.upcallStub(MethodHandles.lookup().findStatic(ForeignProbe.class, "decrement", byteType.toMethodType()), byteType, arena);
            check((int)function(lib, "ffi_byte_callback", FunctionDescriptor.of(JAVA_INT, ADDRESS, JAVA_BYTE)).invokeExact(byteStub, (byte)-100) == -101);
            FunctionDescriptor floatType = FunctionDescriptor.of(JAVA_DOUBLE, JAVA_FLOAT, JAVA_DOUBLE);
            MemorySegment floatStub = linker.upcallStub(MethodHandles.lookup().findStatic(ForeignProbe.class, "sumFloating", floatType.toMethodType()), floatType, arena);
            check((double)function(lib, "ffi_float_callback", FunctionDescriptor.of(JAVA_DOUBLE, ADDRESS, JAVA_FLOAT, JAVA_DOUBLE)).invokeExact(floatStub, 2.5f, 4d) == 6.5d);
            try { linker.downcallHandle(FunctionDescriptor.ofVoid(MemoryLayout.sequenceLayout(2, JAVA_INT))); throw new AssertionError(); }
            catch (IllegalArgumentException expected) { }
        }
        try { long ignored = (long)strlen.invokeExact(dead); throw new AssertionError("Closed argument accepted: " + ignored); }
        catch (IllegalStateException expected) { }
        try { int ignored = (int)staleLibrary.invokeExact(1, 2); throw new AssertionError("Closed library accepted: " + ignored); }
        catch (IllegalStateException expected) { }
        try (Arena live = Arena.ofConfined()) {
            try {
                qsort.invokeExact(live.allocateFrom(JAVA_INT, 2, 1), 2L, 4L, expiredComparator);
                throw new AssertionError("Closed callback accepted");
            } catch (IllegalStateException expected) { }
        }
        stage = "JNI loader lookup";
        System.load(Path.of(args[0]).toAbsolutePath().toString());
        SymbolLookup loaded = SymbolLookup.loaderLookup();
        check(loaded.find("__openldk_missing_symbol__").isEmpty());
        check((int)function(loaded, "ffi_add", FunctionDescriptor.of(JAVA_INT, JAVA_INT, JAVA_INT)).invokeExact(20, 22) == 42);
        System.out.println("FFM strlen, scalar ABI, pointers, structs, variadics, errno, heap aliases, lifetimes, qsort, and native-thread upcalls passed");
    }
}
