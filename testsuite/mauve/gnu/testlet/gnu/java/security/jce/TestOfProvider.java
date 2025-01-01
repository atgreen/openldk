/* TestOfProvider.java -- FIXME: describe
   Copyright (C) 2006 FIXME: your info here
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

package gnu.testlet.gnu.java.security.jce;

import gnu.java.security.Registry;
import gnu.java.security.provider.Gnu;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.security.MessageDigest;
import java.security.Provider;
import java.security.SecureRandom;
import java.security.Security;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.util.Iterator;
import java.util.Random;

/**
 * Conformance tests for the (standard) JCE Provider implementation.
 */
public class TestOfProvider implements Testlet
{
  public void test(TestHarness harness)
  {
    setUp();

    testProviderName(harness);
    testSha(harness);
    testWhirlpool(harness);
    testShaPRNG(harness);
    testWhirlpoolPRNG(harness);
    testGNUSecureRandoms(harness);
  }

  public void testProviderName(TestHarness harness)
  {
    harness.checkPoint("testProviderName");
    Provider us = Security.getProvider(Registry.GNU_SECURITY);
    harness.check(Registry.GNU_SECURITY.equals(us.getName()));
  }

  public void testSha(TestHarness harness)
  {
    harness.checkPoint("testSha");
    try
      {
        MessageDigest md =
            MessageDigest.getInstance("SHA", Registry.GNU_SECURITY);
        harness.check(md != null);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("testSha()");
      }
  }

  public void testWhirlpool(TestHarness harness)
  {
    harness.checkPoint("testWhirlpool");
    try
      {
        MessageDigest md =
            MessageDigest.getInstance("Whirlpool", Registry.GNU_SECURITY);
        harness.check(md != null);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("testWhirlpool()");
      }
  }

  public void testShaPRNG(TestHarness harness)
  {
    harness.checkPoint("testShaPRNG");
    try
      {
        SecureRandom rnd =
            SecureRandom.getInstance("SHA1PRNG", Registry.GNU_SECURITY);
        harness.check(rnd != null);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("testShaPRNG()");
      }
  }

  public void testWhirlpoolPRNG(TestHarness harness)
  {
    harness.checkPoint("testWhirlpoolPRNG");
    try
      {
        SecureRandom rnd =
            SecureRandom.getInstance("WHIRLPOOLPRNG", Registry.GNU_SECURITY);
        harness.check(rnd != null);
      }
    catch (Exception x)
      {
        x.printStackTrace(System.err);
        harness.fail("testWhirlpoolPRNG()");
      }
  }

  public void testGNUSecureRandoms(TestHarness harness)
  {
    harness.checkPoint("testGNUSecureRandoms");
    String rand;
    Random algorithm;
    for (Iterator it = Security.getAlgorithms("SecureRandom").iterator(); it.hasNext();)
      {
        rand = (String) it.next();
        try
          {
            algorithm = null;
            algorithm = SecureRandom.getInstance(rand, Registry.GNU_SECURITY);
            harness.check(algorithm != null, "getInstance("
                                             + String.valueOf(rand) + ")");
          }
        catch (NoSuchProviderException x)
          {
            harness.fail("getInstance(" + String.valueOf(rand) + "): "
                         + String.valueOf(x));
          }
        catch (NoSuchAlgorithmException x)
          {
            harness.fail("getInstance(" + String.valueOf(rand) + "): "
                         + String.valueOf(x));
          }
      }
  }

  private void setUp()
  {
    Security.addProvider(new Gnu()); // dynamically adds our provider
    Security.removeProvider(Registry.GNU_CRYPTO);
    Security.removeProvider(Registry.GNU_SASL);
  }
}