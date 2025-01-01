// Tags: JDK1.4

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

public class IPv6 implements Testlet
{
  public void test(TestHarness h)
  {
    try
      {
	String name ="1010:0:0:0:0:2020:0:1";
	byte[] ipaddr = { 0x10, 0x10, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x00, 0x20, 0x20, 0x0, 0x0, 0x0, 0x1 };
	InetAddress addr = InetAddress.getByAddress(ipaddr);

	h.check(addr.getHostAddress(), name, "wrong ip string");
	h.check(addr.getHostName(), name, "wrong hostname");
	h.check(addr.getCanonicalHostName(), name, "wrong canonical hostname");
	h.check(addr.toString(), name + "/" + name, "wrong string representation");
      }
    catch (UnknownHostException e)
      {
	h.check(false);
      }
  }
}
