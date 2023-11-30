/* TestOfRSAKeyGeneration.java -- 
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

package gnu.testlet.gnu.java.security.key.rsa;

import gnu.java.security.key.rsa.RSAKeyPairGenerator;
import gnu.java.security.sig.rsa.RSA;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.math.BigInteger;
import java.security.KeyPair;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.interfaces.RSAPrivateKey;
import java.security.interfaces.RSAPrivateCrtKey;
import java.security.interfaces.RSAPublicKey;
import java.util.HashMap;
import java.util.Random;

/**
 * Conformance tests for the RSA key-pair generation implementation.
 */
public class TestOfRSAKeyGeneration implements Testlet
{
  private static final BigInteger ZERO = BigInteger.ZERO;

  private static final BigInteger ONE = BigInteger.ONE;

  private RSAKeyPairGenerator kpg = new RSAKeyPairGenerator();

  private HashMap map = new HashMap();

  public void test(TestHarness harness)
  {
    testKeyPairGeneration(harness);
    testRSAParams(harness);
    testRSAPrimitives(harness);
  }

  public void testKeyPairGeneration(TestHarness harness)
  {
    harness.checkPoint("TestOfRSAKeyGeneration.testKeyPairGeneration");
    try
      {
        setUp();

        map.put(RSAKeyPairGenerator.MODULUS_LENGTH, new Integer(530));
        try
          {
            kpg.setup(map);
            harness.fail("L should be >= 1024");
          }
        catch (IllegalArgumentException x)
          {
            harness.check(true, "L should be >= 1024");
          }

        map.put(RSAKeyPairGenerator.MODULUS_LENGTH, new Integer(1024));
        kpg.setup(map);
        KeyPair kp = kpg.generate();

        BigInteger n1 = ((RSAPublicKey) kp.getPublic()).getModulus();

        BigInteger n2 = ((RSAPrivateKey) kp.getPrivate()).getModulus();
        BigInteger p = ((RSAPrivateCrtKey) kp.getPrivate()).getPrimeP();
        BigInteger q = ((RSAPrivateCrtKey) kp.getPrivate()).getPrimeQ();

        harness.check(n1.equals(p.multiply(q)), "n1 == pq");
        harness.check(n2.equals(p.multiply(q)), "n2 == pq");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfRSAPSSSignature.testKeyPairGeneration");
      }
  }

  public void testRSAParams(TestHarness harness)
  {
    harness.checkPoint("TestOfRSAKeyGeneration.testRSAParams");
    try
      {
        setUp();

        map.put(RSAKeyPairGenerator.MODULUS_LENGTH, new Integer(1024));
        kpg.setup(map);
        KeyPair kp = kpg.generate();

        BigInteger n1 = ((RSAPublicKey) kp.getPublic()).getModulus();
        BigInteger e = ((RSAPublicKey) kp.getPublic()).getPublicExponent();

        BigInteger n2 = ((RSAPrivateKey) kp.getPrivate()).getModulus();
        BigInteger d = ((RSAPrivateKey) kp.getPrivate()).getPrivateExponent();

        BigInteger p = ((RSAPrivateCrtKey) kp.getPrivate()).getPrimeP();
        BigInteger q = ((RSAPrivateCrtKey) kp.getPrivate()).getPrimeQ();
        BigInteger dP = ((RSAPrivateCrtKey) kp.getPrivate()).getPrimeExponentP();
        BigInteger dQ = ((RSAPrivateCrtKey) kp.getPrivate()).getPrimeExponentQ();
        BigInteger qInv = ((RSAPrivateCrtKey) kp.getPrivate()).getCrtCoefficient();

        harness.check(n1.bitLength() == 1024, "n1 is a 1024-bit MPI");
        harness.check(n2.bitLength() == 1024, "n2 is a 1024-bit MPI");
        harness.check(n1.equals(n2), "n1 == n2");

        // In a valid RSA private key with this representation, the two factors p
        // and q are the prime factors of the modulus n,
        harness.check(p.isProbablePrime(80), "p is prime");
        harness.check(q.isProbablePrime(80), "q is prime");
        harness.check(n1.equals(p.multiply(q)), "n == pq");

        // dP and dQ are positive integers less than p and q respectively
        BigInteger p_minus_1 = p.subtract(ONE);
        BigInteger q_minus_1 = q.subtract(ONE);
        harness.check(ZERO.compareTo(dP) < 0 && dP.compareTo(p_minus_1) < 0,
                      "0 < dP < p-1");
        harness.check(ZERO.compareTo(dQ) < 0 && dQ.compareTo(q_minus_1) < 0,
                      "0 < dQ < q-1");
        // satisfying
        //    e . dP = 1 (mod p?1);
        //    e . dQ = 1 (mod q?1),
        harness.check(e.multiply(dP).mod(p_minus_1).equals(ONE),
                      "e.dP == 1 (mod p-1)");
        harness.check(e.multiply(dQ).mod(q_minus_1).equals(ONE),
                      "e.dQ == 1 (mod q-1)");

        // and the CRT coefficient qInv is a positive integer less than p
        // satisfying
        //    q . qInv = 1 (mod p).
        harness.check(q.multiply(qInv).mod(p).equals(ONE),
                      "q.qInv == 1 (mod p)");

        BigInteger phi = p_minus_1.multiply(q_minus_1);
        harness.check(e.gcd(phi).equals(ONE), "gcd(e, phi) == 1");
        harness.check(e.multiply(d).mod(phi).equals(ONE), "e.d == 1 (mod phi)");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfRSAPSSSignature.testRSAParams");
      }
  }

  public void testRSAPrimitives(TestHarness harness)
  {
    harness.checkPoint("TestOfRSAKeyGeneration.testRSAPrimitives");
    try
      {
        setUp();

        map.put(RSAKeyPairGenerator.MODULUS_LENGTH, new Integer(1024));
        kpg.setup(map);
        KeyPair kp = kpg.generate();

        PublicKey pubK = kp.getPublic();
        PrivateKey privK = kp.getPrivate();

        BigInteger n = ((RSAPublicKey) pubK).getModulus();
        BigInteger m = ZERO;
        Random prng = new Random(System.currentTimeMillis());
        while (m.equals(ZERO) || m.compareTo(n) >= 0)
          m = new BigInteger(1024, prng);

        BigInteger s = RSA.sign(privK, m);
        BigInteger cm = RSA.verify(pubK, s);

        harness.check(cm.equals(m));
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfRSAPSSSignature.testRSAPrimitives");
      }
  }

  private void setUp()
  {
    kpg = new RSAKeyPairGenerator();
    map.clear();
  }
}