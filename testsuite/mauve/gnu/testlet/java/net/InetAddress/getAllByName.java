// Tags: JDK1.0

/*
   This file is part of Mauve.

   Mauve is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Mauve is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Mauve; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.
*/

package gnu.testlet.java.net.InetAddress;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.net.*;


public class getAllByName implements Testlet
{
  public void test(TestHarness harness)
  {
    // getAllByName(null) should return the loopback address.
    try
    {
      InetAddress[] addr = InetAddress.getAllByName(null);
      harness.check(addr != null);
      InetAddress loopback = addr[0];
      harness.check(loopback != null);
      harness.check(loopback.isLoopbackAddress());
    }
    catch (UnknownHostException x)
    {
      harness.fail(x.toString());
    }

    // so should getAllByName("")
    try
      {
        InetAddress[] addr = InetAddress.getAllByName("");
        harness.check(addr != null);
        InetAddress loopback = addr[0];
        harness.check(loopback != null);
        harness.check(loopback.isLoopbackAddress());
      }
    catch (UnknownHostException x)
      {
        harness.fail(x.toString());
      }
  }
}
