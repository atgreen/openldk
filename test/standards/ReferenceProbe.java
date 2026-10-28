// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.lang.ref.*;
import java.util.concurrent.atomic.AtomicInteger;
public class ReferenceProbe {
    static void check(boolean v) { if (!v) throw new AssertionError(); }
    static WeakReference<Object> abandoned(ReferenceQueue<Object> queue) {
        return new WeakReference<>(new Object(), queue);
    }
    static PhantomReference<Object> phantom(ReferenceQueue<Object> queue) {
        return new PhantomReference<>(new Object(), queue);
    }
    static void abandonedCleanable(Cleaner cleaner, AtomicInteger count) {
        cleaner.register(new Object(), count::incrementAndGet);
    }
    public static void main(String[] args) throws Exception {
        Object value = new Object();
        var queue = new ReferenceQueue<Object>();
        var weak = new WeakReference<>(value, queue);
        check(weak.refersTo(value) && weak.get() == value);
        weak.clear(); check(weak.refersTo(null) && weak.get() == null);
        check(queue.poll() == null);
        check(weak.enqueue()); check(!weak.enqueue()); check(queue.remove(1000) == weak);
        var phantom = new PhantomReference<>(value, queue);
        check(phantom.get() == null && phantom.refersTo(value));
        phantom.clear(); check(phantom.refersTo(null));
        var count = new AtomicInteger();
        var cleaner = Cleaner.create();
        {
            var cleanable = cleaner.register(value, count::incrementAndGet);
            cleanable.clean(); cleanable.clean(); check(count.get() == 1);
        }
        abandonedCleanable(cleaner, count);
        var soft = new SoftReference<>(value);
        check(soft.get() == value);
        var gcQueue = new ReferenceQueue<Object>();
        var abandoned = abandoned(gcQueue);
        var deadPhantom = phantom(gcQueue);
        boolean weakQueued = false, phantomQueued = false;
        for (int i = 0; i < 100 && !(weakQueued && phantomQueued && count.get() == 2); i++) {
            System.gc();
            Reference<?> ref;
            while ((ref = gcQueue.poll()) != null) {
                if (ref == abandoned) weakQueued = true;
                if (ref == deadPhantom) phantomQueued = true;
            }
            Thread.sleep(20);
        }
        check(weakQueued && phantomQueued && abandoned.get() == null && count.get() == 2);
        soft.clear(); check(soft.refersTo(null));
        Reference.reachabilityFence(value);
        System.out.println("References: identity, clear/enqueue, GC delivery and automatic/idempotent Cleaner passed");
    }
}
