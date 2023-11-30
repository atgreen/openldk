/* TestOfOtherProviders.java -- 
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

package gnu.testlet.gnu.javax.crypto.jce;

import gnu.java.security.Registry;
import gnu.javax.crypto.jce.GnuCrypto;
import gnu.javax.crypto.jce.GnuSasl;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.security.Provider;
import java.security.SecureRandom;
import java.security.Security;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.util.Iterator;
import java.util.Random;

/**
 * Conformance tests for the JCE Provider implementation.
 */
public class TestOfOtherProviders implements Testlet
{
  public void test(TestHarness harness)
  {
    setUp();

    testProviderName(harness);
    testOtherSecureRandoms(harness);
  }

  public void testProviderName(TestHarness harness)
  {
    harness.checkPoint("testProviderName");
    Provider us = Security.getProvider(Registry.GNU_CRYPTO);
    harness.check(Registry.GNU_CRYPTO.equals(us.getName()));
    us = Security.getProvider(Registry.GNU_SASL);
    harness.check(Registry.GNU_SASL.equals(us.getName()));
  }

  public void testOtherSecureRandoms(TestHarness harness)
  {
    harness.checkPoint("testOtherSecureRandoms");
    String rand;
    Random algorithm;
//    for (Iterator it = Gnu.getSecureRandomNames().iterator(); it.hasNext();)
    for (Iterator it = Security.getAlgorithms("SecureRandom").iterator(); it.hasNext();)
      {
        rand = (String) it.next();
        try
          {
            algorithm = null;
            algorithm = SecureRandom.getInstance(rand, Registry.GNU_CRYPTO);
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
    Security.removeProvider(Registry.GNU_SECURITY);
    Security.addProvider(new GnuCrypto()); // dynamically adds our provider
    Security.addProvider(new GnuSasl()); // dynamically adds our provider
  }
}