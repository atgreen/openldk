// Tags: JDK1.4

// Copyright (C) 2005 David Daney <ddaney@avtrex.com>

// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.

// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.net.InetAddress;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.net.InetAddress;
import java.net.UnknownHostException;

/**
 * Some checks for the getByAddress() and getAddress() methods in the 
 * {@link InetAddress} class.
 */
public class getByAddress implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)   
  {
    testX(harness);
  }
  
  /**
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  private void testX(TestHarness harness) {
    byte[] a1 = new byte[] {1, 2, 3, 4};
    InetAddress ia = null;
    try
      {
        ia = InetAddress.getByAddress(a1);
      }
    catch (UnknownHostException uhe)
      {
        harness.check(false);
      }

    byte[] a2 = ia.getAddress();
    harness.check(a2[0], (byte)1);
    harness.check(a2[1], (byte)2);
    harness.check(a2[2], (byte)3);
    harness.check(a2[3], (byte)4);
    harness.check(a2 != a1);

    a1[0] = 5;
    byte[] a3 = ia.getAddress();
    harness.check(a3[0], (byte)1);
    harness.check(a2 != a3);


    byte[] a4 = new byte[] {1, 2, 3, 4, 5, 6, 7, 8,
                            9, 10, 11, 12, 13, 14, 15, 16};
    ia = null;
    try
      {
        ia = InetAddress.getByAddress(a4);
      }
    catch (UnknownHostException uhe)
      {
        harness.check(false);
      }

    byte[] a5 = ia.getAddress();
    harness.check(a5[0], (byte)1);
    harness.check(a5[1], (byte)2);
    harness.check(a5[2], (byte)3);
    harness.check(a5[15], (byte)16);
    harness.check(a5 != a4);

    a4[0] = 5;
    byte[] a6 = ia.getAddress();
    harness.check(a6[0], (byte)1);
    harness.check(a5 != a6);
  }
}
