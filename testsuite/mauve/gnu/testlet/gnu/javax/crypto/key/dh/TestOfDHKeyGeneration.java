/* TestOfDHKeyGeneration.java -- 
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

package gnu.testlet.gnu.javax.crypto.key.dh;

import gnu.javax.crypto.key.dh.GnuDHKeyPairGenerator;
import gnu.javax.crypto.key.dh.GnuDHPrivateKey;
import gnu.javax.crypto.key.dh.GnuDHPublicKey;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.math.BigInteger;
import java.security.KeyPair;
import java.util.HashMap;

/**
 * Conformance tests for the Diffie-Hellman key-pair generation implementation.
 */
public class TestOfDHKeyGeneration implements Testlet
{
  public void test(TestHarness harness)
  {
    harness.checkPoint("TestOfDHKeyGeneration");
    GnuDHKeyPairGenerator kpg = new GnuDHKeyPairGenerator();
    HashMap map = new HashMap();
    map.put(GnuDHKeyPairGenerator.PRIME_SIZE, new Integer(530));

    try
      {
        kpg.setup(map);
        harness.fail("L should be <= 1024 and of the form 512 + 64n");
      }
    catch (IllegalArgumentException x)
      {
        harness.check(true, "L should be <= 1024 and of the form 512 + 64n");
      }

    map.put(GnuDHKeyPairGenerator.PRIME_SIZE, new Integer(512));
    map.put(GnuDHKeyPairGenerator.EXPONENT_SIZE, new Integer(160));
    kpg.setup(map);
    KeyPair kp = kpg.generate();

    BigInteger p1 = ((GnuDHPublicKey) kp.getPublic()).getParams().getP();
    BigInteger p2 = ((GnuDHPrivateKey) kp.getPrivate()).getParams().getP();
    harness.check(p1.equals(p2), "p1.equals(p2)");

    BigInteger q1 = ((GnuDHPublicKey) kp.getPublic()).getQ();
    BigInteger q2 = ((GnuDHPrivateKey) kp.getPrivate()).getQ();
    harness.check(q1.equals(q2), "q1.equals(q2)");

    BigInteger g1 = ((GnuDHPublicKey) kp.getPublic()).getParams().getG();
    BigInteger g2 = ((GnuDHPrivateKey) kp.getPrivate()).getParams().getG();
    harness.check(g1.equals(g2), "g1.equals(g2)");

    harness.check(p1.isProbablePrime(80), "p is probable prime");
  }
}