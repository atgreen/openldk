/* Consistency.java -- test NetworkInterface API consistency.
   Copyright (C) 2006  Casey Marshall <csm@gnu.org>
This file is part of Mauve.

Mauve is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Mauve is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Mauve; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.  */

// Tags: JDK1.4

package gnu.testlet.java.net.NetworkInterface;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.net.InetAddress;
import java.net.NetworkInterface;
import java.util.Enumeration;
import java.util.HashSet;

/**
 * @author csm
 *
 */
public class Consistency
  implements Testlet
{

  /* (non-Javadoc)
   * @see gnu.testlet.Testlet#test(gnu.testlet.TestHarness)
   */
  public void test(TestHarness harness)
  {
    Enumeration ifs = null;
    harness.checkPoint("getNetworkInterfaces");
    try
      {
        ifs = NetworkInterface.getNetworkInterfaces();
        harness.check(ifs != null);
      }
    catch (Exception x)
      {
        harness.fail("getNetworkInterfaces");
        harness.debug(x);
        return;
      }
    
    harness.check(ifs.hasMoreElements(), "getNetworkInterfaces returns something");
    
    HashSet names = new HashSet();
    while (ifs.hasMoreElements())
      {
        NetworkInterface netif = (NetworkInterface) ifs.nextElement();
        harness.checkPoint("consistency - " + netif.getName());
        harness.check(!names.contains(netif.getName()), "duplicate entries");
        names.add(netif.getName());

        try
          {
            NetworkInterface netif2 = NetworkInterface.getByName(netif.getName());
            harness.check(netif2 != null);
            harness.check(netif.equals(netif2));
          }
        catch (Exception x)
          {
            harness.fail("getByName unexpected exception");
            harness.debug(x);
          }
        
        Enumeration addrs = netif.getInetAddresses();
        harness.check(addrs.hasMoreElements());
        
        while (addrs.hasMoreElements())
          {
            try
              {
                InetAddress addr = (InetAddress) addrs.nextElement();
                NetworkInterface netif2 = NetworkInterface.getByInetAddress(addr);
                harness.check(netif2 != null);
                harness.check(netif.equals(netif2));
              }
            catch (Exception x)
              {
                harness.fail("getByAddress unexpected exception");
                harness.debug(x);
              }
          }
      }
  }
}
