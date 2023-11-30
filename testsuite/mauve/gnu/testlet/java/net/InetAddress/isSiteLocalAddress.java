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

public class isSiteLocalAddress implements Testlet
{
  public void test(TestHarness h)
  {
    try
      {
	// 10.0.0.0/8
	h.check(InetAddress.getByAddress(new byte[] { (byte) 10, (byte) 0,  (byte) 0, (byte) 0 }).isSiteLocalAddress());
	h.check(InetAddress.getByAddress(new byte[] { (byte) 10, (byte) 255, (byte) 255, (byte) 255 }).isSiteLocalAddress());

	h.check(! InetAddress.getByAddress(new byte[] { (byte) 9, (byte) 255,  (byte) 255, (byte) 255 }).isSiteLocalAddress());
	h.check(! InetAddress.getByAddress(new byte[] { (byte) 11, (byte) 0,  (byte) 0, (byte) 0 }).isSiteLocalAddress());

	// 172.16.0.0/12
	h.check(InetAddress.getByAddress(new byte[] { (byte) 172, (byte) 16, (byte) 0, (byte) 0 }).isSiteLocalAddress());
	h.check(InetAddress.getByAddress(new byte[] { (byte) 172, (byte) 31, (byte) 255, (byte) 255 }).isSiteLocalAddress());
	
	h.check(! InetAddress.getByAddress(new byte[] { (byte) 172, (byte) 15, (byte) 255, (byte) 255 }).isSiteLocalAddress());
	h.check(! InetAddress.getByAddress(new byte[] { (byte) 172, (byte) 32, (byte) 0, (byte) 0 }).isSiteLocalAddress());
	
	// 192.168.0.0/16
	h.check(InetAddress.getByAddress(new byte[] { (byte) (byte) 192, (byte) 168, (byte) 0, (byte) 0 }).isSiteLocalAddress());
	h.check(InetAddress.getByAddress(new byte[] { (byte) (byte) 192, (byte) 168, (byte) 255, (byte) 255 }).isSiteLocalAddress());

	h.check(! InetAddress.getByAddress(new byte[] { (byte) (byte) 192, (byte) 167, (byte) 255, (byte) 255 }).isSiteLocalAddress());
	h.check(! InetAddress.getByAddress(new byte[] { (byte) (byte) 192, (byte) 169, (byte) 0, (byte) 0 }).isSiteLocalAddress());
      }
    catch (UnknownHostException e)
      {
	h.debug(e);
	h.check(false, "unexpected exception");
      }
  }
}
