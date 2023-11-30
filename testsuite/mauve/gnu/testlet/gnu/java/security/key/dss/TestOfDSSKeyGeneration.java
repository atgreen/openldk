/* TestOfDSSKeyGeneration.java -- 
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

package gnu.testlet.gnu.java.security.key.dss;

import gnu.java.security.key.dss.DSSKeyPairGenerator;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.math.BigInteger;
import java.security.KeyPair;
import java.security.interfaces.DSAPrivateKey;
import java.security.interfaces.DSAPublicKey;
import java.util.HashMap;

/**
 * Conformance tests for the DSS key-pair generation implementation.
 */
public class TestOfDSSKeyGeneration implements Testlet
{
  public void test(TestHarness harness)
  {
    harness.checkPoint("TestOfDSSKeyGeneration");
    DSSKeyPairGenerator kpg = new DSSKeyPairGenerator();
    HashMap map = new HashMap();
    map.put(DSSKeyPairGenerator.MODULUS_LENGTH, new Integer(530));

    try
      {
        kpg.setup(map);
        harness.fail("L should be <= 1024 and of the form 512 + 64n");
      }
    catch (IllegalArgumentException x)
      {
        harness.check(true, "L should be <= 1024 and of the form 512 + 64n");
      }

    map.put(DSSKeyPairGenerator.MODULUS_LENGTH, new Integer(512));
    map.put(DSSKeyPairGenerator.USE_DEFAULTS, new Boolean(false));
    kpg.setup(map);
    KeyPair kp = kpg.generate();

    BigInteger p1 = ((DSAPublicKey) kp.getPublic()).getParams().getP();
    BigInteger p2 = ((DSAPrivateKey) kp.getPrivate()).getParams().getP();
    harness.check(p1.equals(p2), "p1.equals(p2)");

    BigInteger q1 = ((DSAPublicKey) kp.getPublic()).getParams().getQ();
    BigInteger q2 = ((DSAPrivateKey) kp.getPrivate()).getParams().getQ();
    harness.check(q1.equals(q2), "q1.equals(q2)");

    BigInteger g1 = ((DSAPublicKey) kp.getPublic()).getParams().getG();
    BigInteger g2 = ((DSAPrivateKey) kp.getPrivate()).getParams().getG();
    harness.check(g1.equals(g2), "g1.equals(g2)");

    harness.check(q1.isProbablePrime(80), "q is probable prime");
    harness.check(p1.isProbablePrime(80), "p is probable prime");
  }
}