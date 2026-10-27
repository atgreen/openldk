/* { dg-output "Reference processing passed" } */
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
package aaa;

import java.lang.ref.*;

public class referenceprocessing {
    static volatile boolean cleaned;
    static volatile int cleanupCalls;
    static Cleaner.Cleanable registration(Cleaner cleaner) {
        return cleaner.register(new Object(), () -> { cleanupCalls++; cleaned = true; });
    }
    static void check(boolean condition, String message) {
        if (!condition) throw new AssertionError(message);
    }
    static WeakReference<Object> weak(ReferenceQueue<Object> queue) {
        return new WeakReference<>(new Object(), queue);
    }
    static PhantomReference<Object> phantom(ReferenceQueue<Object> queue) {
        return new PhantomReference<>(new Object(), queue);
    }
    static void await(Reference<?> reference, ReferenceQueue<Object> queue) throws Exception {
        // refersTo(null) does not temporarily strengthen the referent, unlike get().
        for (int i = 0; i < 40; i++) {
            System.gc();
            Thread.sleep(25);
            if (reference.refersTo(null)) break;
        }
        check(reference.refersTo(null), "referent was retained");
        check(queue.remove(5000) == reference, "GC did not enqueue reference");
        check(queue.poll() == null, "duplicate enqueue");
        check(!reference.enqueue(), "reenqueued removed reference");
    }
    public static void main(String[] args) throws Exception {
        ReferenceQueue<Object> queue = new ReferenceQueue<>();
        Object live = new Object();
        WeakReference<Object> weak = new WeakReference<>(live, queue);
        SoftReference<Object> soft = new SoftReference<>(live);
        PhantomReference<Object> phantom = new PhantomReference<>(live, queue);
        System.gc();
        check(weak.get() == live && soft.get() == live, "live referent cleared");
        check(phantom.get() == null && phantom.refersTo(live), "phantom identity");
        Reference.reachabilityFence(live);
        weak.clear();
        check(weak.get() == null && weak.refersTo(null), "explicit clear failed");
        check(queue.poll() == null, "clear enqueued reference");
        check(weak.enqueue(), "manual enqueue failed");
        check(queue.remove(1000) == weak, "manual queue delivery failed");
        phantom.clear();
        check(phantom.refersTo(null), "phantom clear failed");
        WeakReference<Object> cleared = weak(queue);
        cleared.clear();
        System.gc();
        check(queue.remove(100) == null, "GC enqueued explicitly cleared reference");
        Reference.reachabilityFence(cleared);
        await(weak(queue), queue);
        await(phantom(queue), queue);
        check(new WeakReference<Object>(null).refersTo(null), "null referent");
        Cleaner cleaner = Cleaner.create();
        Cleaner.Cleanable cleanable = registration(cleaner);
        // Cold compilation and verification on the Cleaner thread can outlast
        // two seconds under load. Allow asynchronous cleanup time to complete.
        for (int i = 0; i < 200 && !cleaned; i++) {
            System.gc();
            Thread.sleep(50);
        }
        check(cleaned, "Cleaner did not run");
        cleanable.clean();
        check(cleanupCalls == 1, "Cleaner ran more than once");
        Reference.reachabilityFence(cleanable);
        System.out.println("Reference processing passed");
    }
}
