/* { dg-output "OK" } */

package aaa;

// Object.wait() releases the monitor and wakes a thread on the monitor's
// condition variable, which threads entering share with threads in wait().
// Waking only one could hand the wakeup to another wait(), leaving a thread
// asleep at the door of a free monitor until that wait timed out.
public class waitwake
{
    static final Object lock = new Object();
    static volatile long released = -1;
    static volatile long entered = -1;

    static class Waiter extends Thread
    {
        public void run()
        {
            synchronized (lock) {
                try { lock.wait(5000); } catch (InterruptedException e) {}
            }
        }
    }

    static class Holder extends Thread
    {
        public void run()
        {
            synchronized (lock) {
                try {
                    Thread.sleep(300);
                    released = System.currentTimeMillis();
                    lock.wait(5000);   // releases the monitor to the enterer
                } catch (InterruptedException e) {}
            }
        }
    }

    static class Enterer extends Thread
    {
        public void run()
        {
            synchronized (lock) {
                entered = System.currentTimeMillis();
                lock.notifyAll();
            }
        }
    }

    public static void main(String[] args) throws Exception
    {
        Waiter waiter = new Waiter();
        waiter.start();
        Thread.sleep(100);           // the waiter sits in wait(5000), monitor free
        Holder holder = new Holder();
        holder.start();
        Thread.sleep(100);           // the holder owns the monitor for 300 ms
        Enterer enterer = new Enterer();
        enterer.start();             // blocks entering
        enterer.join(10000);
        long delay = entered - released;
        System.out.println(entered >= 0 && delay < 3000 ? "OK" : "slow after " + delay + " ms");
    }
}
