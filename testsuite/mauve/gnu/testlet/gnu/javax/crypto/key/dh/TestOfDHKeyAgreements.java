/* TestOfDHKeyAgreements.java -- 
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

import gnu.java.security.Registry;
import gnu.java.security.key.IKeyPairGenerator;
import gnu.java.security.key.KeyPairGeneratorFactory;
import gnu.javax.crypto.key.IKeyAgreementParty;
import gnu.javax.crypto.key.KeyAgreementException;
import gnu.javax.crypto.key.IncomingMessage;
import gnu.javax.crypto.key.OutgoingMessage;
import gnu.javax.crypto.key.dh.DiffieHellmanKeyAgreement;
import gnu.javax.crypto.key.dh.DiffieHellmanReceiver;
import gnu.javax.crypto.key.dh.DiffieHellmanSender;
import gnu.javax.crypto.key.dh.ElGamalReceiver;
import gnu.javax.crypto.key.dh.ElGamalSender;
import gnu.javax.crypto.key.dh.ElGamalKeyAgreement;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.security.KeyPair;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

/**
 * A test case for the Diffie-Hellman key agreements, both the Static-Static
 * (basic version) and the Ephemeral-Static (ElGamal) modes.
 */
public class TestOfDHKeyAgreements implements Testlet
{
  private KeyPair kpA, kpB;

  private IKeyAgreementParty A, B;

  public void test(TestHarness harness)
  {
    testOfStaticStatic(harness);
    testOfEphemeralStatic(harness);
  }

  public void testOfStaticStatic(TestHarness harness)
  {
    harness.checkPoint("TestOfDHKeyAgreements.testOfStaticStatic");

    setUp();

    A = new DiffieHellmanSender();
    B = new DiffieHellmanReceiver();

    Map mapA = new HashMap();
    mapA.put(DiffieHellmanKeyAgreement.KA_DIFFIE_HELLMAN_OWNER_PRIVATE_KEY,
             kpA.getPrivate());
    Map mapB = new HashMap();
    mapB.put(DiffieHellmanKeyAgreement.KA_DIFFIE_HELLMAN_OWNER_PRIVATE_KEY,
             kpB.getPrivate());

    try
      {
        A.init(mapA);
      }
    catch (KeyAgreementException x)
      {
        harness.debug(x);
        harness.fail("while initialising A");
      }
    harness.check(!A.isComplete(), "A is ready");

    try
      {
        B.init(mapB);
      }
    catch (KeyAgreementException x)
      {
        harness.debug(x);
        harness.fail("while initialising B");
      }
    harness.check(!B.isComplete(), "B is ready");

    // (1) A -> B: g**x mod p
    OutgoingMessage out = null;
    try
      {
        out = A.processMessage(null);
      }
    catch (KeyAgreementException x)
      {
        harness.debug(x);
        harness.fail("while A is in step #1");
      }
    harness.check(!A.isComplete(), "A is OK after step #1");

    // (2) B -> A: g^^y mod p
    IncomingMessage in = null;
    try
      {
        in = new IncomingMessage(out.toByteArray());
      }
    catch (KeyAgreementException x)
      {
        harness.debug(x);
        harness.fail("while feeding B, A's incoming message");
      }
    out = null;
    try
      {
        out = B.processMessage(in);
      }
    catch (KeyAgreementException x)
      {
        harness.debug(x);
        harness.fail("while B is in step #1");
      }
    harness.check(B.isComplete(), "B is complete after step #1");

    byte[] k2 = null;
    try
      {
        k2 = B.getSharedSecret();
      }
    catch (KeyAgreementException x)
      {
        harness.fail("while accessing B's version of the shared secret");
      }

    // A computes the shared secret
    in = null;
    try
      {
        in = new IncomingMessage(out.toByteArray());
      }
    catch (KeyAgreementException x)
      {
        harness.debug(x);
        harness.fail("while feeding A, B's incoming message");
      }
    out = null;
    try
      {
        out = A.processMessage(in);
      }
    catch (KeyAgreementException x)
      {
        harness.debug(x);
        harness.fail("while A is in step #2");
      }
    harness.check(A.isComplete(), "A is complete after step #2");

    byte[] k1 = null;
    try
      {
        k1 = A.getSharedSecret();
      }
    catch (KeyAgreementException x)
      {
        harness.fail("while accessing A's version of the shared secret");
      }

    harness.check(Arrays.equals(k1, k2), "A and B share the same secret");
  }

  public void testOfEphemeralStatic(TestHarness harness)
  {
    harness.checkPoint("TestOfDHKeyAgreements.testOfEphemeralStatic");

    setUp();

    A = new ElGamalSender();
    B = new ElGamalReceiver();

    Map mapA = new HashMap();
    mapA.put(ElGamalKeyAgreement.KA_ELGAMAL_RECIPIENT_PUBLIC_KEY,
             kpB.getPublic());
    Map mapB = new HashMap();
    mapB.put(ElGamalKeyAgreement.KA_ELGAMAL_RECIPIENT_PRIVATE_KEY,
             kpB.getPrivate());

    try
      {
        A.init(mapA);
      }
    catch (KeyAgreementException x)
      {
        harness.debug(x);
        harness.fail("while initialising A");
      }
    harness.check(!A.isComplete(), "A is ready");

    try
      {
        B.init(mapB);
      }
    catch (KeyAgreementException x)
      {
        harness.debug(x);
        harness.fail("while initialising B");
      }
    harness.check(!B.isComplete(), "B is ready");

    // (1) A -> B: g**x mod p
    OutgoingMessage out = null;
    try
      {
        out = A.processMessage(null);
      }
    catch (KeyAgreementException x)
      {
        harness.debug(x);
        harness.fail("while A is in step #1");
      }
    harness.check(A.isComplete(), "A is complete after step #1");

    IncomingMessage in = null;
    try
      {
        in = new IncomingMessage(out.toByteArray());
      }
    catch (KeyAgreementException x)
      {
        harness.debug(x);
        harness.fail("while feeding B, A's incoming message");
      }
    out = null;
    try
      {
        out = B.processMessage(in);
      }
    catch (KeyAgreementException x)
      {
        harness.debug(x);
        harness.fail("while B is in step #1");
      }
    harness.check(B.isComplete(), "B is complete after step #1");

    byte[] k1 = null;
    try
      {
        k1 = A.getSharedSecret();
      }
    catch (KeyAgreementException x)
      {
        harness.fail("while accessing A's version of the shared secret");
      }

    byte[] k2 = null;
    try
      {
        k2 = B.getSharedSecret();
      }
    catch (KeyAgreementException x)
      {
        harness.fail("while accessing B's version of the shared secret");
      }

    harness.check(Arrays.equals(k1, k2), "A and B share the same secret");
  }

  private void setUp()
  {
    IKeyPairGenerator kpg = KeyPairGeneratorFactory.getInstance(Registry.DH_KPG);
    kpg.setup(new HashMap()); // use default values
    kpA = kpg.generate();
    kpB = kpg.generate();
  }
}