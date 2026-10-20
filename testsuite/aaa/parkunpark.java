/* { dg-output "OK" } */

package aaa;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.LockSupport;

// LockSupport.park/unpark on platform threads: unpark wakes a parked thread
// promptly, a permit given first makes park return at once, a timed park
// returns after its timeout, and interrupt wakes a parked thread without
// an exception.  A cached thread pool hands a second task to an idle worker
// this way.
public class parkunpark
{
    static volatile boolean woke;

    public static void main(String[] args) throws Exception
    {
        // unpark wakes a parked thread.
        Thread t = new Thread(() -> { LockSupport.park(); woke = true; });
        t.start();
        Thread.sleep(100);
        long start = System.nanoTime();
        LockSupport.unpark(t);
        t.join(5000);
        long wakeMs = (System.nanoTime() - start) / 1000000;
        boolean unparkOk = woke && wakeMs < 2000;

        // A permit given before park makes park return at once.
        LockSupport.unpark(Thread.currentThread());
        start = System.nanoTime();
        LockSupport.park();
        boolean permitOk = (System.nanoTime() - start) / 1000000 < 1000;

        // A timed park returns after about its timeout.
        start = System.nanoTime();
        LockSupport.parkNanos(200000000L);
        long timedMs = (System.nanoTime() - start) / 1000000;
        boolean timedOk = timedMs >= 150 && timedMs < 3000;

        // interrupt wakes a parked thread, which sees itself interrupted.
        boolean[] sawInterrupt = new boolean[1];
        woke = false;
        Thread u = new Thread(() -> { LockSupport.park(); sawInterrupt[0] = Thread.currentThread().isInterrupted(); woke = true; });
        u.start();
        Thread.sleep(100);
        u.interrupt();
        u.join(5000);
        boolean interruptOk = woke && sawInterrupt[0];

        // A cached pool hands a second task to its idle worker.
        ExecutorService ex = Executors.newCachedThreadPool(r -> { Thread d = new Thread(r); d.setDaemon(true); return d; });
        String first = ex.submit(() -> "one").get();
        Thread.sleep(200);
        String second = ex.submit(() -> "two").get(10, TimeUnit.SECONDS);
        boolean poolOk = first.equals("one") && second.equals("two");

        boolean ok = unparkOk && permitOk && timedOk && interruptOk && poolOk;
        System.out.println(ok ? "OK" : "FAIL: unpark=" + unparkOk + "(" + wakeMs + "ms) permit=" + permitOk
                           + " timed=" + timedOk + "(" + timedMs + "ms) interrupt=" + interruptOk + " pool=" + poolOk);
    }
}
