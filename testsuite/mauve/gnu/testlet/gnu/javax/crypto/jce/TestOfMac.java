/* TestOfMac.java -- 
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
import gnu.java.security.prng.IRandom;
import gnu.java.security.prng.MDGenerator;
import gnu.javax.crypto.cipher.CipherFactory;
import gnu.javax.crypto.cipher.IBlockCipher;
import gnu.javax.crypto.mac.IMac;
import gnu.javax.crypto.mac.MacFactory;
import gnu.javax.crypto.mac.TMMH16;
import gnu.javax.crypto.mac.UMac32;
import gnu.javax.crypto.jce.GnuCrypto;
import gnu.javax.crypto.jce.spec.TMMHParameterSpec;
import gnu.javax.crypto.jce.spec.UMac32ParameterSpec;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.security.Security;
import java.security.spec.AlgorithmParameterSpec;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;

import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;

/**
 * Conformance tests for the JCE Provider implementations of MAC SPI classes.
 */
public class TestOfMac implements Testlet
{
  public void test(TestHarness harness)
  {
    setUp();

    testUnknownMac(harness);
    testEquality(harness);
    testCloneability(harness);
  }

  /** Should fail with an unknown algorithm. */
  public void testUnknownMac(TestHarness harness)
  {
    harness.checkPoint("testUnknownMac");
    try
      {
        Mac.getInstance("Godot", Registry.GNU_CRYPTO);
        harness.fail("testUnknownMac()");
      }
    catch (Exception x)
      {
        harness.check(true);
      }
  }

  /**
   * Tests if the result of using a MAC through gnu.crypto Factory classes
   * yields same value as using instances obtained the JCE way.
   */
  public void testEquality(TestHarness harness)
  {
    harness.checkPoint("testEquality");
    String macName;
    IMac gnu = null;
    Mac jce = null;
    byte[] in = this.getClass().getName().getBytes();
    byte[] ba1, ba2;
    HashMap attrib = new HashMap();
    for (Iterator it = MacFactory.getNames().iterator(); it.hasNext();)
      {
        macName = (String) it.next();
        // we dont provide OMAC based on NullCipher through the JCE. skip
        if (macName.equals("omac-null"))
          continue;

        AlgorithmParameterSpec params = null;
        if (macName.equalsIgnoreCase("UMAC32"))
          {
            byte[] nonce = new byte[16];
            for (int i = 0; i < nonce.length; i++)
              nonce[i] = (byte) i;

            params = new UMac32ParameterSpec(nonce);
            attrib.put(UMac32.NONCE_MATERIAL, nonce);
          }
        else if (macName.equalsIgnoreCase("TMMH16"))
          {
            IRandom rand1 = new MDGenerator();
            rand1.init(new HashMap());
            Integer tagLen = new Integer(4);
            params = new TMMHParameterSpec(rand1, tagLen);

            IRandom rand2 = new MDGenerator();
            rand2.init(new HashMap());
            attrib.put(TMMH16.KEYSTREAM, rand2);
            attrib.put(TMMH16.TAG_LENGTH, tagLen);
          }

        try
          {
            gnu = MacFactory.getInstance(macName);
            harness.check(gnu != null, "MacFactory.getInstance(" + macName
                                       + ")");
          }
        catch (InternalError x)
          {
            harness.fail("MacFactory.getInstance(" + macName + "): "
                         + String.valueOf(x));
          }

        try
          {
            jce = Mac.getInstance(macName, Registry.GNU_CRYPTO);
            harness.check(jce != null, "Mac.getInstance()");
          }
        catch (Exception x)
          {
            harness.debug(x);
            harness.fail("Mac.getInstance(" + macName + "): "
                         + String.valueOf(x));
          }

        byte[] kb = null;
        if (macName.equalsIgnoreCase("UMAC32")
            || macName.equalsIgnoreCase("UHASH32"))
          kb = new byte[16];
        else if (macName.toLowerCase().startsWith(Registry.OMAC_PREFIX))
          {
            IBlockCipher cipher = CipherFactory.getInstance(
                macName.substring(Registry.OMAC_PREFIX.length()));
            if (cipher != null)
              kb = new byte[cipher.defaultKeySize()];
            else
              kb = new byte[gnu.macSize()];
          }
        else
          kb = new byte[gnu.macSize()];

        for (int i = 0; i < kb.length; i++)
          kb[i] = (byte) i;

        attrib.put(IMac.MAC_KEY_MATERIAL, kb);
        try
          {
            gnu.init(attrib);
            if (macName.equalsIgnoreCase("TMMH16"))
              jce.init(null, params);
            else
              jce.init(new SecretKeySpec(kb, macName), params);
          }
        catch (Exception x)
          {
            harness.debug(x);
            harness.fail("Mac.getInstance(" + macName + "): "
                         + String.valueOf(x));
          }

        gnu.update(in, 0, in.length);
        ba1 = gnu.digest();
        ba2 = jce.doFinal(in);

        harness.check(Arrays.equals(ba1, ba2), "testEquality(" + macName + ")");
      }
  }

  /**
   * Tests if the result of a cloned, partially in-progress hash instance,
   * when used later to further process data, yields the same result as the
   * original copy.
   */
  public void testCloneability(TestHarness harness)
  {
    harness.checkPoint("testCloneability");
    String macName;
    Mac mac1, mac2;
    byte[] abc = "abc".getBytes();
    byte[] in = this.getClass().getName().getBytes();
    byte[] ba1, ba2;
    for (Iterator it = MacFactory.getNames().iterator(); it.hasNext();)
      {
        macName = (String) it.next();
        // no point in testing OMAC-based MACs since they are not Cloneable
        if (macName.startsWith(Registry.OMAC_PREFIX))
          continue;

        try
          {
            AlgorithmParameterSpec params = null;
            if (macName.equalsIgnoreCase("UMAC32"))
              {
                byte[] nonce = new byte[16];
                for (int i = 0; i < nonce.length; i++)
                  nonce[i] = (byte) i;

                params = new UMac32ParameterSpec(nonce);
              }
            else if (macName.equalsIgnoreCase("TMMH16"))
              {
                IRandom rand = new MDGenerator();
                rand.init(new HashMap());
                Integer tagLen = new Integer(4);
                params = new TMMHParameterSpec(rand, tagLen);
              }

            mac1 = Mac.getInstance(macName, Registry.GNU_CRYPTO);
            byte[] kb = null;
            if (macName.equalsIgnoreCase("UMAC32")
                || macName.equalsIgnoreCase("UHASH32"))
              kb = new byte[16];
            else
              kb = new byte[mac1.getMacLength()];

            for (int i = 0; i < kb.length; i++)
              kb[i] = (byte) i;

            if (macName.equalsIgnoreCase("TMMH16"))
              mac1.init(null, params);
            else
              mac1.init(new SecretKeySpec(kb, macName), params);

            mac1.update(abc); // start with abc
            mac2 = (Mac) mac1.clone(); // now clone it

            ba1 = mac1.doFinal(in); // now finish both with in
            ba2 = mac2.doFinal(in);

            harness.check(Arrays.equals(ba1, ba2), "testCloneability("
                                                   + macName + ")");
          }
        catch (Exception x)
          {
            harness.debug(x);
            harness.fail("testCloneability(" + macName + "): "
                         + String.valueOf(x));
          }
      }
  }

  private void setUp()
  {
    Security.addProvider(new GnuCrypto()); // dynamically adds our provider
  }
}