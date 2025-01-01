/* TestOfHMac.java -- 
   Copyright (C) 2006 Free Software Foundation, Inc.
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
02110-1301 USA.

*/

// Tags: GNU-CRYPTO JDK1.4

package gnu.testlet.gnu.javax.crypto.mac;

import gnu.javax.crypto.mac.HMacFactory;
import gnu.javax.crypto.mac.IMac;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Random;

/**
 * Conformance Tests of common characteristics to all HMAC types in this
 * library; e.g. cloning.
 */
public class TestOfHMac implements Testlet
{
  private static final Random prng = new Random(System.currentTimeMillis());

  private String mac;

  private IMac algorithm, clone;

  private static final byte[] makeKey(int length)
  {
    byte[] result = new byte[length];
    prng.nextBytes(result);
    return result;
  }

  public void test(TestHarness harness)
  {
    harness.checkPoint("TestOfHMac");
    HashMap attr = new HashMap();
    for (Iterator it = HMacFactory.getNames().iterator(); it.hasNext();)
      {
        algorithm = null;
        mac = (String) it.next();
        try
          {
            algorithm = HMacFactory.getInstance(mac);
            harness.check(algorithm != null, "getInstance("
                                             + String.valueOf(mac) + ")");
          }
        catch (InternalError x)
          {
            harness.debug(x);
            harness.fail("TestOfHMac.getInstance(" + String.valueOf(mac)
                         + ") - " + String.valueOf(x));
          }

        // cloneable
        attr.put(IMac.MAC_KEY_MATERIAL, makeKey(algorithm.macSize()));
        try
          {
            algorithm.init(attr);
            algorithm.update((byte) 'a');
            algorithm.update((byte) 'b');
            algorithm.update((byte) 'c');

            clone = (IMac) algorithm.clone();

            algorithm.update((byte) 'd');
            clone.update((byte) 'd');
            algorithm.update((byte) 'e');
            clone.update((byte) 'e');
            algorithm.update((byte) 'f');
            clone.update((byte) 'f');

            byte[] md1 = algorithm.digest();
            byte[] md2 = clone.digest();

            harness.check(Arrays.equals(md1, md2), "clone(" + algorithm.name()
                                                   + ")");
          }
        catch (Exception x)
          {
            harness.debug(x);
            harness.fail("TestOfHMac.clone(" + algorithm.name() + ") - "
                         + String.valueOf(x));
          }

        // reusable
        try
          {
            algorithm.init(attr);
            algorithm.update((byte) 'a');
            algorithm.update((byte) 'b');
            algorithm.update((byte) 'c');
            byte[] md1 = algorithm.digest();

            algorithm.reset();
            algorithm.update((byte) 'a');
            algorithm.update((byte) 'b');
            algorithm.update((byte) 'c');
            byte[] md2 = algorithm.digest();

            harness.check(Arrays.equals(md1, md2), "reset(" + algorithm.name()
                                                   + ")");
          }
        catch (Exception x)
          {
            harness.debug(x);
            harness.fail("TestOfHMac.reset(" + algorithm.name() + ") - "
                         + String.valueOf(x));
          }
      }
  }
}