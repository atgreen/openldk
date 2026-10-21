/* { dg-output "OK" } */

package aaa;

import java.lang.management.*;

// ManagementFactory's MXBeans come up, and the current thread's CPU time can
// be measured (the Kotlin compiler times its phases this way).
public class threadcpu
{
    public static void main(String[] args)
    {
        ThreadMXBean t = ManagementFactory.getThreadMXBean();
        t.setThreadCpuTimeEnabled(true);
        long c1 = t.getCurrentThreadCpuTime();
        long x = 0;
        for (int i = 0; i < 3000000; i++) x += i % 7;
        long c2 = t.getCurrentThreadCpuTime();
        RuntimeMXBean r = ManagementFactory.getRuntimeMXBean();
        boolean ok = t.isCurrentThreadCpuTimeSupported() && c1 > 0 && c2 > c1 && x > 0
            && t.getThreadCount() > 0 && r.getUptime() >= 0 && r.getPid() > 0;
        System.out.println(ok ? "OK" : "FAIL: " + c1 + " " + c2 + " " + t.getThreadCount());
    }
}
