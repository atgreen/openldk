// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import sun.misc.Unsafe;

// Used by make check-reference-image: the first call warms the image; the
// second call, in a different process, must clear and enqueue the saved reference.
public class ReferenceImage {
    static Object root;
    static WeakReference<Object> reference;
    static ReferenceQueue<Object> queue;
    static Unsafe unsafe;
    static long memory;

    public static void main(String[] args) throws Exception {
        if (reference == null) {
            var field = Unsafe.class.getDeclaredField("theUnsafe");
            field.setAccessible(true);
            unsafe = (Unsafe) field.get(null);
            memory = unsafe.allocateMemory(64);
            unsafe.putLong(memory, 0x123456789abcdefL);
            unsafe.putLong(memory + 56, 0x76543210fedcbaL);
            memory = unsafe.reallocateMemory(memory, 128);
            if (unsafe.getLong(memory) != 0x123456789abcdefL
                || unsafe.getLong(memory + 56) != 0x76543210fedcbaL)
                throw new AssertionError("Native reallocation lost contents");
            unsafe.putLong(memory + 120, 0xabcdef12345678L);
            root = new Object();
            queue = new ReferenceQueue<>();
            reference = new WeakReference<>(root, queue);
        } else {
            if (unsafe.getLong(memory) != 0x123456789abcdefL
                || unsafe.getLong(memory + 56) != 0x76543210fedcbaL
                || unsafe.getLong(memory + 120) != 0xabcdef12345678L)
                throw new AssertionError("Saved native memory was not restored");
            unsafe.freeMemory(memory);
            memory = unsafe.allocateMemory(128);
            unsafe.putLong(memory, 42);
            if (unsafe.getLong(memory) != 42) throw new AssertionError("Restored allocation could not be reused");
            unsafe.freeMemory(memory);
            memory = 0;
            root = null;
            for (int i = 0; i < 40; i++) {
                System.gc();
                Thread.sleep(25);
                if (reference.refersTo(null)) break;
            }
            if (queue.remove(5000) != reference)
                throw new AssertionError("saved reference was not collected and enqueued");
            System.out.println("Reference image passed");
        }
    }
}
