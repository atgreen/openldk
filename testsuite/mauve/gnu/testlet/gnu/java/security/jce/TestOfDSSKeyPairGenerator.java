/* TestOfDSSKeyPairGenerator.java
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

package gnu.testlet.gnu.java.security.jce;

import java.math.BigInteger;
import java.security.AlgorithmParameters;
import java.security.InvalidParameterException;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.Provider;
import java.security.SecureRandom;
import java.security.Security;
import java.security.interfaces.DSAKeyPairGenerator;
import java.security.spec.AlgorithmParameterSpec;
import java.security.spec.DSAParameterSpec;

import gnu.java.security.Registry;
import gnu.java.security.provider.Gnu;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * API conformance tests for the DSS key-pair generator.
 */
public class TestOfDSSKeyPairGenerator implements Testlet
{
  private SecureRandom rnd = new SecureRandom();
  private KeyPairGenerator kpg;

  public void test(TestHarness harness)
  {
    setUp(harness);

    checkInvalidDefaultModuli(harness);
    checkValidDefaultModuli(harness);
    checkInitializeSignatures(harness);
  }

  private void setUp(TestHarness harness)
  {
    Security.addProvider(new Gnu()); // dynamically adds our provider
    try
      {
        kpg = KeyPairGenerator.getInstance(Registry.DSS_KPG, Registry.GNU_SECURITY);
      }
    catch (NoSuchAlgorithmException x)
      {
        harness.debug(x);
        harness.fail("setUp(): " + x.getMessage());
      }
    catch (NoSuchProviderException x)
      {
        harness.debug(x);
        harness.fail("setUp(): " + x.getMessage());
      }
  }

  private void checkInvalidDefaultModuli(TestHarness harness)
  {
    harness.checkPoint("checkInvalidDefaultModuli");
    for (int i = 512 + 64; i < 1024; i += 64)
      if (i % 256 == 0)
        continue;
      else
        {
          if (initModLen(i))
            harness.fail(i + "-bit IS NOT a valid modulus length");
        }

    harness.check(true, "Unsupported default values are rejected");
  }

  private boolean initModLen(int modlen)
  {
    boolean result = false;
    try
      {
        kpg.initialize(modlen);
        result = true;
      }
    catch (Exception ignored)
      {
      }

    return result;
  }

  private void checkValidDefaultModuli(TestHarness harness)
  {
    harness.checkPoint("checkValidDefaultModuli");

    for (int i = 0, modlen = 512; i < 2; i++, modlen += 256 * i)
      if (!initModLen(modlen))
        harness.fail(modlen + "-bit MUST be a valid modulus length");

    harness.check(true, "Supported default values are accepted");
  }

  private void checkInitializeSignatures(TestHarness harness)
  {
    harness.checkPoint("checkInitializeSignatures");

    String msg;

    // KeyPairGenerator init methods ------------------------------------------

    Provider fp = new Gnu();

    msg = "initialize(AlgorithmParameterSpec) MUST succeed";
    try
      {
        AlgorithmParameters ap = AlgorithmParameters.getInstance("DSA", fp);
        AlgorithmParameterSpec spec =
            ap.getParameterSpec(DSAParameterSpec.class);
        kpg.initialize(spec);
        harness.check(true, msg);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail(msg);
      }

    msg = "initialize(AlgorithmParameterSpec, SecureRandom) MUST succeed";
    try
      {
        AlgorithmParameters ap = AlgorithmParameters.getInstance("DSA", fp);
        AlgorithmParameterSpec spec =
            ap.getParameterSpec(DSAParameterSpec.class);
        kpg.initialize(spec, rnd);
        harness.check(true, msg);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail(msg);
      }

    msg= "initialize(512) MUST succeed";
    try
      {
        kpg.initialize(512);
        harness.check(true, msg);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail(msg);
      }

    msg= "initialize(512, SecureRandom) MUST succeed";
    try
      {
        kpg.initialize(512, rnd);
        harness.check(true, msg);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail(msg);
      }

    // DSAKeyPairGenerator init methods ---------------------------------------

    DSAKeyPairGenerator dsakpg = (DSAKeyPairGenerator) kpg;
    msg = "initialize(DSAParams, SecureRandom) MUST succeed";
    try
      {
        dsakpg.initialize(
            new DSAParameterSpec(BigInteger.ONE, BigInteger.ONE, BigInteger.ONE),
            rnd);
        harness.check(true, msg);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail(msg);
      }

    msg = "initialize(null, SecureRandom) MUST throw InvalidParameterException";
    try
      {
        dsakpg.initialize(null, rnd);
        harness.fail(msg);
      }
    catch (InvalidParameterException x)
      {
        harness.check(true, msg);
      }
    catch (Exception x)
    {
      harness.debug(x);
      harness.fail(msg);
    }

    msg = "initialize(512, false, SecureRandom) MUST succeed";
    try
      {
        dsakpg.initialize(512, false, rnd);
        harness.check(true, msg);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail(msg);
      }

    msg = "initialize(1024, false, SecureRandom) MUST succeed";
    try
      {
        dsakpg.initialize(512, false, rnd);
        harness.check(true, msg);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail(msg);
      }
  }
}
