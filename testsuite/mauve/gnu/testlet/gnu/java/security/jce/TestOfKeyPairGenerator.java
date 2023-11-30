/* TestOfKeyPairGenerator.java -- 
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

import gnu.java.security.Registry;
import gnu.java.security.provider.Gnu;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.math.BigInteger;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.Security;
import java.security.interfaces.DSAPrivateKey;
import java.security.interfaces.DSAPublicKey;
import java.security.interfaces.RSAPrivateCrtKey;
import java.security.interfaces.RSAPrivateKey;
import java.security.interfaces.RSAPublicKey;

/**
 * Conformance tests for the JCE keypair generation implementations.<p>
 */
public class TestOfKeyPairGenerator implements Testlet
{
  public void test(TestHarness harness)
  {
    setUp();

    testUnknownGenerator(harness);
    checkJCEKeyPairGenerator(harness, "DSA");
    checkJCEKeyPairGenerator(harness, "DSS");
    testRSAKeyPairGenerator(harness);
  }

  private void setUp()
  {
    Security.addProvider(new Gnu()); // dynamically adds our provider
  }

  /** Should fail with an unknown algorithm. */
  private void testUnknownGenerator(TestHarness harness)
  {
    harness.checkPoint("testUnknownGenerator");
    try
      {
        KeyPairGenerator.getInstance("ABC", Registry.GNU_SECURITY);
        harness.fail("testUnknownGenerator()");
      }
    catch (Exception x)
      {
        harness.check(true);
      }
  }

  private void checkJCEKeyPairGenerator(TestHarness harness, String algo)
  {
    harness.checkPoint("checkJCEKeyPairGenerator - " + algo);
    try
      {
        KeyPairGenerator kpg =
            KeyPairGenerator.getInstance(algo, Registry.GNU_SECURITY);

        String msg = "L MUST be <= 1024 and of the form 512 + 64n";
        try
          {
            kpg.initialize(530);
            harness.fail(msg);
          }
        catch (IllegalArgumentException x)
          {
            harness.check(true, msg);
          }

        kpg.initialize(512);
        KeyPair kp = kpg.generateKeyPair();

        BigInteger p1 = ((DSAPublicKey) kp.getPublic()).getParams().getP();
        harness.check(p1.isProbablePrime(80), "p is probable prime - 512-bit");
        BigInteger p2 = ((DSAPrivateKey) kp.getPrivate()).getParams().getP();
        harness.check(p1.equals(p2), "p1.equals(p2) - 512-bit");

        BigInteger q1 = ((DSAPublicKey) kp.getPublic()).getParams().getQ();
        harness.check(q1.isProbablePrime(80), "q is probable prime - 512-bit");
        BigInteger q2 = ((DSAPrivateKey) kp.getPrivate()).getParams().getQ();
        harness.check(q1.equals(q2), "q1.equals(q2) - 512-bit");

        BigInteger g1 = ((DSAPublicKey) kp.getPublic()).getParams().getG();
        BigInteger g2 = ((DSAPrivateKey) kp.getPrivate()).getParams().getG();
        harness.check(g1.equals(g2), "g1.equals(g2) - 512-bit");

        kp = kpg.generateKeyPair();
        p2 = ((DSAPublicKey) kp.getPublic()).getParams().getP();
        harness.check(p1.equals(p2), "MUST provide default params for L = 512-bit (p)");
        q2 = ((DSAPublicKey) kp.getPublic()).getParams().getQ();
        harness.check(p1.equals(p2), "MUST provide default params for L = 512-bit (q)");
        g2 = ((DSAPublicKey) kp.getPublic()).getParams().getG();
        harness.check(p1.equals(p2), "MUST provide default params for L = 512-bit (g)");


        kpg.initialize(1024);
        kp = kpg.generateKeyPair();

        p1 = ((DSAPublicKey) kp.getPublic()).getParams().getP();
        harness.check(p1.isProbablePrime(80), "p is probable prime - 1024-bit");
        p2 = ((DSAPrivateKey) kp.getPrivate()).getParams().getP();
        harness.check(p1.equals(p2), "p1.equals(p2) - 1024-bit");

        q1 = ((DSAPublicKey) kp.getPublic()).getParams().getQ();
        harness.check(q1.isProbablePrime(80), "q is probable prime - 1024-bit");
        q2 = ((DSAPrivateKey) kp.getPrivate()).getParams().getQ();
        harness.check(q1.equals(q2), "q1.equals(q2) - 1024-bit");

        g1 = ((DSAPublicKey) kp.getPublic()).getParams().getG();
        g2 = ((DSAPrivateKey) kp.getPrivate()).getParams().getG();
        harness.check(g1.equals(g2), "g1.equals(g2) - 1024-bit");

        kp = kpg.generateKeyPair();
        p2 = ((DSAPublicKey) kp.getPublic()).getParams().getP();
        harness.check(p1.equals(p2), "MUST provide default params for L = 1024-bit (p)");
        q2 = ((DSAPublicKey) kp.getPublic()).getParams().getQ();
        harness.check(p1.equals(p2), "MUST provide default params for L = 1024-bit (q)");
        g2 = ((DSAPublicKey) kp.getPublic()).getParams().getG();
        harness.check(p1.equals(p2), "MUST provide default params for L = 1024-bit (g)");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("checkJCEKeyPairGenerator - " + algo);
      }
  }

  private void testRSAKeyPairGenerator(TestHarness harness)
  {
    harness.checkPoint("testRSAKeyPairGenerator");
    try
      {
        KeyPairGenerator kpg =
            KeyPairGenerator.getInstance("RSA", Registry.GNU_SECURITY);

        // modulus length should be at least 1024
        try
          {
            kpg.initialize(1023);
            harness.fail("L should be >= 1024");
          }
        catch (IllegalArgumentException x)
          {
            harness.check(true, "L should be >= 1024");
          }

        kpg.initialize(1024);
        KeyPair kp = kpg.generateKeyPair();

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
        harness.fail("testRSAKeyPairGenerator()");
      }
  }
}