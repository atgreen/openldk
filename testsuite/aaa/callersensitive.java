/* { dg-output "OK" } */

package aaa;

import java.util.concurrent.atomic.AtomicIntegerFieldUpdater;
import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;

// Caller-sensitive methods see the class that called them: an atomic field
// updater may reach a private field of its caller's class, or of a nestmate
// (Kotlin's lazy uses one).  OpenLDK reported the updater's own class as the
// caller, which may not.
public class callersensitive
{
    private volatile Object ref;

    static class Inner { private volatile int count; }

    public static void main(String[] args) throws Exception
    {
        AtomicReferenceFieldUpdater<callersensitive, Object> refs =
            AtomicReferenceFieldUpdater.newUpdater(callersensitive.class, Object.class, "ref");
        callersensitive c = new callersensitive();
        refs.set(c, "x");
        AtomicIntegerFieldUpdater<Inner> counts = AtomicIntegerFieldUpdater.newUpdater(Inner.class, "count");
        Inner i = new Inner();
        counts.incrementAndGet(i);
        String result = c.ref + " " + i.count + " " + Class.forName("aaa.callersensitive").getSimpleName();
        System.out.println("x 1 callersensitive".equals(result) ? "OK" : "FAIL: " + result);
    }
}
