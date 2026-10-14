/* { dg-output "OK" } */

package aaa;

import java.time.Instant;

// Instant.now() reads the clock through VM.getNanoTimeAdjustment.
public class instant
{
    public static void main(String[] args)
    {
        long fromInstant = Instant.now().toEpochMilli();
        long fromSystem = System.currentTimeMillis();
        System.out.println(Math.abs(fromInstant - fromSystem) < 5000 ? "OK" : "off by " + (fromInstant - fromSystem) + " ms");
    }
}
