/* TestOfSRPKeyGeneration.java -- 
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

package gnu.testlet.gnu.javax.crypto.key.srp6;

import gnu.javax.crypto.key.srp6.SRPKeyPairGenerator;
import gnu.javax.crypto.key.srp6.SRPPrivateKey;
import gnu.javax.crypto.key.srp6.SRPPublicKey;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.math.BigInteger;
import java.security.KeyPair;
import java.util.HashMap;

/**
 * Conformance tests for the SRP key-pair generation implementation.
 */
public class TestOfSRPKeyGeneration implements Testlet
{
  public void test(TestHarness harness)
  {
    harness.checkPoint("TestOfSRPKeyGeneration");
    SRPKeyPairGenerator kpg = new SRPKeyPairGenerator();
    HashMap map = new HashMap();
    map.put(SRPKeyPairGenerator.MODULUS_LENGTH, new Integer(530));

    try
      {
        kpg.setup(map);
        harness.fail("L should be >= 512, <= 2048 and of the form 512 + 256n");
      }
    catch (IllegalArgumentException x)
      {
        harness.check(true,
                      "L should be >= 512, <= 2048 and of the form 512 + 256n");
      }

    map.put(SRPKeyPairGenerator.MODULUS_LENGTH, new Integer(512));
    map.put(SRPKeyPairGenerator.USE_DEFAULTS, Boolean.FALSE);
    kpg.setup(map);
    KeyPair kp = kpg.generate();

    BigInteger N1 = ((SRPPublicKey) kp.getPublic()).getN();
    BigInteger N2 = ((SRPPrivateKey) kp.getPrivate()).getN();
    harness.check(N1.equals(N2), "N1.equals(N2)");

    BigInteger q = N1.subtract(BigInteger.ONE).divide(BigInteger.valueOf(2L));

    BigInteger g1 = ((SRPPublicKey) kp.getPublic()).getG();
    BigInteger g2 = ((SRPPrivateKey) kp.getPrivate()).getG();
    harness.check(g1.equals(g2), "g1.equals(g2)");

    harness.check(N1.isProbablePrime(80), "N is probable prime");
    harness.check(q.isProbablePrime(80), "q is probable prime");
  }
}