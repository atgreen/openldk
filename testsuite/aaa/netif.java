/* { dg-output "OK" } */

package aaa;

import java.net.*;
import java.security.SecureRandom;
import java.util.*;

// java.net.NetworkInterface: SHA1PRNG's seed generator reads every
// interface's description and hardware address (Spring MVC's flash map
// manager seeds one on a POST's redirect).
public class netif
{
    public static void main(String[] args) throws Exception
    {
        List<String> got = new ArrayList<>();
        List<NetworkInterface> all = Collections.list(NetworkInterface.getNetworkInterfaces());
        NetworkInterface lo = NetworkInterface.getByName("lo");
        got.add(String.valueOf(all.contains(lo)));
        got.add(lo.getName() + " up=" + lo.isUp() + " loopback=" + lo.isLoopback() + " p2p=" + lo.isPointToPoint()
                + " mac=" + Arrays.toString(lo.getHardwareAddress()) + " mtu>0=" + (lo.getMTU() > 0));
        got.add(String.valueOf(Collections.list(lo.getInetAddresses()).contains(InetAddress.getByName("127.0.0.1"))));
        InterfaceAddress ia = lo.getInterfaceAddresses().stream()
            .filter(a -> a.getAddress() instanceof Inet4Address).findFirst().get();
        got.add(ia.getAddress().getHostAddress() + "/" + ia.getNetworkPrefixLength());
        got.add(String.valueOf(NetworkInterface.getByInetAddress(InetAddress.getByName("127.0.0.1")).equals(lo)));
        got.add(String.valueOf(NetworkInterface.getByIndex(lo.getIndex()).equals(lo)));
        got.add(String.valueOf(NetworkInterface.getByName("no-such-interface")));
        got.add(String.valueOf(lo.getSubInterfaces().hasMoreElements()));
        SecureRandom random = SecureRandom.getInstance("SHA1PRNG");
        random.nextInt();
        got.add("seeded");
        String expected = "[true, lo up=true loopback=true p2p=false mac=null mtu>0=true, true, 127.0.0.1/8, "
            + "true, true, null, false, seeded]";
        System.out.println(expected.equals(got.toString()) ? "OK" : got.toString());
    }
}
