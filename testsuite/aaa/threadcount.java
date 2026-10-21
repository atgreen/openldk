/* { dg-output "OK" } */

package aaa;

// Thread.activeCount and enumerate go through the native Thread.getThreads.
public class threadcount
{
    public static void main(String[] args) throws Exception
    {
        Object lock = new Object();
        Thread t = new Thread(() -> { synchronized (lock) { try { lock.wait(); } catch (InterruptedException e) {} } });
        t.setDaemon(true);
        t.start();
        Thread.sleep(200);
        Thread[] all = new Thread[Thread.activeCount() + 4];
        int n = Thread.enumerate(all);
        boolean main = false, other = false;
        for (int i = 0; i < n; i++) {
            if (all[i] == Thread.currentThread()) main = true;
            if (all[i] == t) other = true;
        }
        System.out.println(main && other ? "OK" : "FAIL: n=" + n + " main=" + main + " other=" + other);
        synchronized (lock) { lock.notifyAll(); }
    }
}
