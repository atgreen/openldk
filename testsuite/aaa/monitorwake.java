/* { dg-output "OK" } */

package aaa;

// Exiting a monitor wakes a thread on the monitor's condition variable, which
// threads entering share with threads in Object.wait().  Waking only one could
// hand the wakeup to a wait(), leaving a thread asleep at the door of a free
// monitor until the wait timed out.
public class monitorwake
{
    static final Object lock = new Object();
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
                try { Thread.sleep(300); } catch (InterruptedException e) {}
            }
        }
    }

    static class Enterer extends Thread
    {
        public void run()
        {
            long before = System.currentTimeMillis();
            synchronized (lock) {
                entered = System.currentTimeMillis() - before;
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
        System.out.println(entered >= 0 && entered < 3000 ? "OK" : "slow after " + entered + " ms");
    }
}
