/* { dg-output "OK" } */

package aaa;

// Four threads make the first call of the same method at once.  The compile
// finishing wakes waiters on a condition variable every method shares; waking
// only one left the other threads asleep, with nothing coming to wake them.
public class jitwake extends Thread
{
    static volatile boolean go;
    static final long[] out = new long[4];
    final int id;

    jitwake(int id) { this.id = id; }

    static long work(int i)
    {
        long s = 0;
        for (int k = 0; k < 1000; k++) s += k;
        return s + i;
    }

    public void run()
    {
        while (!go) { }
        out[id] = work(id);
    }

    public static void main(String[] args) throws Exception
    {
        // Warm every path main uses after the race, so the only compiles
        // left are the ones the racing threads contend on.
        Thread warm = new Thread();
        warm.start();
        warm.join(1000);
        Thread.sleep(1);

        jitwake[] threads = new jitwake[4];
        for (int i = 0; i < 4; i++) threads[i] = new jitwake(i);
        for (jitwake t : threads) t.start();
        Thread.sleep(500);
        go = true;
        for (jitwake t : threads) t.join(20000);

        boolean ok = true;
        for (int i = 0; i < 4; i++) ok &= out[i] == 499500 + i;
        System.out.println(ok ? "OK" : "stuck");
    }
}
