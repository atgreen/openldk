/* TestOfHMacCloneability.java
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

import gnu.java.security.Registry;
import gnu.javax.crypto.mac.HMacFactory;
import gnu.javax.crypto.mac.IMac;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Random;

/**
 * Tests implementation of the Cloneable interface for GNU HMACs.
 */
public class TestOfHMacCloneability implements Testlet
{
  private static final Random prng = new Random(System.currentTimeMillis());
  private static final String HMAC_NAME = Registry.HMAC_NAME_PREFIX
                                          + Registry.SHA_HASH;
  private IMac hmac;
  HashMap hmacAttributes = new HashMap();

  /* (non-Javadoc)
   * @see gnu.testlet.Testlet#test(gnu.testlet.TestHarness)
   */
  public void test(TestHarness harness)
  {
    clone1(harness);
    clone2(harness);
  }

  /** Test of an un-initialised clone. */
  private void clone1(TestHarness harness)
  {
    harness.checkPoint("TestOfHMacCloneability.clone1");

    setUp(harness);
    try
      {
        IMac clone = (IMac) hmac.clone();

        hmac.init(hmacAttributes);

        hmac.update((byte) 'a');
        hmac.update((byte) 'b');
        hmac.update((byte) 'c');

        clone.init(hmacAttributes);

        clone.update((byte) 'a');
        clone.update((byte) 'b');
        clone.update((byte) 'c');

        byte[] md1 = hmac.digest();
        byte[] md2 = clone.digest();

        harness.check(Arrays.equals(md1, md2), "clone1");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfHMacCloneability.clone1 - " + x);
      }
  }

  /** Test of an initialised clone. */
  private void clone2(TestHarness harness)
  {
    harness.checkPoint("TestOfHMacCloneability.clone1");

    setUp(harness);
    try
      {
        hmac.init(hmacAttributes);

        hmac.update((byte) 'a');
        hmac.update((byte) 'b');
        hmac.update((byte) 'c');

        IMac clone = (IMac) hmac.clone();

        hmac.update((byte) 'd');
        hmac.update((byte) 'e');
        hmac.update((byte) 'f');

        clone.update((byte) 'd');
        clone.update((byte) 'e');
        clone.update((byte) 'f');

        byte[] md1 = hmac.digest();
        byte[] md2 = clone.digest();

        harness.check(Arrays.equals(md1, md2), "clone2");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfHMacCloneability.clone2 - " + x);
      }
  }

  private void setUp(TestHarness harness)
  {
    try
      {
        hmac = HMacFactory.getInstance(HMAC_NAME);
        harness.check(hmac != null, "getInstance(" + HMAC_NAME + ")");
      }
    catch (Throwable t)
      {
        harness.debug(t);
        harness.fail("getInstance(" + HMAC_NAME + ") - " + t);
      }

    // make a key
    int hmacLength = hmac.macSize();
    byte[] keyMaterial = new byte[hmacLength];
    prng.nextBytes(keyMaterial);

    hmacAttributes.clear();
    hmacAttributes.put(IMac.MAC_KEY_MATERIAL, keyMaterial);
  }
}
