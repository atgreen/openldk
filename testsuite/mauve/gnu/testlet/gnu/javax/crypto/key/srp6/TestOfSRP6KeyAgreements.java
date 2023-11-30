/* TestOfSRP6KeyAgreements.java -- 
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

import gnu.java.security.Registry;
import gnu.java.security.util.Util;
import gnu.javax.crypto.key.IKeyAgreementParty;
import gnu.javax.crypto.key.IncomingMessage;
import gnu.javax.crypto.key.KeyAgreementException;
import gnu.javax.crypto.key.OutgoingMessage;
import gnu.javax.crypto.key.srp6.SRP6Host;
import gnu.javax.crypto.key.srp6.SRP6KeyAgreement;
import gnu.javax.crypto.key.srp6.SRP6User;
import gnu.javax.crypto.key.srp6.SRP6SaslClient;
import gnu.javax.crypto.key.srp6.SRP6SaslServer;
import gnu.javax.crypto.sasl.srp.SRPAuthInfoProvider;
import gnu.javax.crypto.sasl.srp.SRPRegistry;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.io.IOException;
import java.io.File;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;

import javax.security.sasl.AuthenticationException;

/**
 * A test case for the SRP-6, both basic and SASL alternative versions of
 * the key agreement protocol.
 */
public class TestOfSRP6KeyAgreements implements Testlet
{
  private String I = "test"; // user name

  private String p = "test"; // user plain password

  private String pFile = "./gnu_crypto_srp6test";

  private String p2File = pFile + "2"; // ./test2

  private String cFile = pFile + ".conf"; // ./test.conf

  private SRPAuthInfoProvider tpasswd = new SRPAuthInfoProvider();

  private IKeyAgreementParty A, B;

  BigInteger N;

  BigInteger g;

  private Random prng = new Random();

  public void test(final TestHarness harness)
  {
    testBasicVersion(harness);
    testSaslVersion(harness);
  }

  public void testBasicVersion(final TestHarness harness)
  {
    harness.checkPoint("TestOfSRP6KeyAgreements.testBasicVersion");

    try
      {
        setUp();
      }
    catch (IOException x)
      {
        harness.debug(x);
        harness.fail("while setting up the test");
      }

    A = new SRP6User();

    final Map mapA = new HashMap();
    mapA.put(SRP6KeyAgreement.SHARED_MODULUS, N);
    mapA.put(SRP6KeyAgreement.GENERATOR, g);
    mapA.put(SRP6KeyAgreement.HASH_FUNCTION, Registry.MD5_HASH);
    mapA.put(SRP6KeyAgreement.USER_IDENTITY, I);
    mapA.put(SRP6KeyAgreement.USER_PASSWORD, p.getBytes());

    try
      {
        A.init(mapA);
      }
    catch (KeyAgreementException x)
      {
        harness.debug(x);
        harness.fail("while initialising User");
      }
    harness.check(!A.isComplete(), "User is ready");

    B = new SRP6Host();

    final Map mapB = new HashMap();
    mapB.put(SRP6KeyAgreement.SHARED_MODULUS, N);
    mapB.put(SRP6KeyAgreement.GENERATOR, g);
    mapB.put(SRP6KeyAgreement.HASH_FUNCTION, Registry.MD5_HASH);
    mapB.put(SRP6KeyAgreement.HOST_PASSWORD_DB, tpasswd);

    try
      {
        B.init(mapB);
      }
    catch (KeyAgreementException x)
      {
        harness.debug(x);
        harness.fail("while initialising Host");
      }
    harness.check(!B.isComplete(), "Host is ready");

    // (1) user send I and it's public ephemeral key
    OutgoingMessage out = null;
    try
      {
        out = A.processMessage(null);
      }
    catch (KeyAgreementException x)
      {
        harness.debug(x);
        harness.fail("while User (A) is in step #1");
      }
    harness.check(!A.isComplete(), "User (A) is OK after step #1");

    // (2) host receives user identity and key, and generates its own
    IncomingMessage in = null;
    try
      {
        in = new IncomingMessage(out.toByteArray());
      }
    catch (KeyAgreementException x)
      {
        harness.debug(x);
        harness.fail("while feeding Host (B), User's (A) incoming message");
      }
    out = null;
    try
      {
        out = B.processMessage(in);
      }
    catch (KeyAgreementException x)
      {
        harness.debug(x);
        harness.fail("while Host (B) is in step #1");
      }
    harness.check(B.isComplete(), "Host (B) is complete after step #1");

    byte[] k2 = null;
    try
      {
        k2 = B.getSharedSecret();
      }
    catch (KeyAgreementException x)
      {
        harness.fail("while accessing Host's (B) version of the shared secret");
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
        harness.fail("while feeding User (A), Host's (B) incoming message");
      }
    //      out = null;
    try
      {
        //         out = A.processMessage(in);
        A.processMessage(in);
      }
    catch (KeyAgreementException x)
      {
        harness.debug(x);
        harness.fail("while User (A) is in step #2");
      }
    harness.check(A.isComplete(), "User (A) is complete after step #2");

    byte[] k1 = null;
    try
      {
        k1 = A.getSharedSecret();
      }
    catch (KeyAgreementException x)
      {
        harness.fail("while accessing User's (A) version of the shared secret");
      }

    harness.check(Arrays.equals(k1, k2),
                  "User (A) and Host (B) share the same secret");

    tearDown();
  }

  public void testSaslVersion(final TestHarness harness)
  {
    harness.checkPoint("TestOfSRP6KeyAgreements.testSaslVersion");

    try
      {
        setUp();
      }
    catch (IOException x)
      {
        harness.debug(x);
        harness.fail("while setting up the test");
      }

    A = new SRP6SaslClient();

    final Map mapA = new HashMap();
    mapA.put(SRP6KeyAgreement.HASH_FUNCTION, Registry.MD5_HASH);
    mapA.put(SRP6KeyAgreement.USER_IDENTITY, I);
    mapA.put(SRP6KeyAgreement.USER_PASSWORD, p.getBytes());

    try
      {
        A.init(mapA);
      }
    catch (KeyAgreementException x)
      {
        harness.debug(x);
        harness.fail("while initialising Client");
      }
    harness.check(!A.isComplete(), "Client is ready");

    B = new SRP6SaslServer();

    final Map mapB = new HashMap();
    mapB.put(SRP6KeyAgreement.HASH_FUNCTION, Registry.MD5_HASH);
    mapB.put(SRP6KeyAgreement.HOST_PASSWORD_DB, tpasswd);

    try
      {
        B.init(mapB);
      }
    catch (KeyAgreementException x)
      {
        harness.debug(x);
        harness.fail("while initialising Server");
      }
    harness.check(!B.isComplete(), "Server is ready");

    // (1) user send I
    OutgoingMessage out = null;
    try
      {
        out = A.processMessage(null);
      }
    catch (KeyAgreementException x)
      {
        harness.debug(x);
        harness.fail("while Client (A) is in step #1");
      }
    harness.check(!A.isComplete(), "Client (A) is OK after step #1");

    // (2) host receives user identity, and generates its own public key
    IncomingMessage in = null;
    try
      {
        in = new IncomingMessage(out.toByteArray());
      }
    catch (KeyAgreementException x)
      {
        harness.debug(x);
        harness.fail("while feeding Server (B), Client's (A) incoming message");
      }
    out = null;
    try
      {
        out = B.processMessage(in);
      }
    catch (KeyAgreementException x)
      {
        harness.debug(x);
        harness.fail("while Server (B) is in step #1");
      }
    harness.check(!B.isComplete(), "Server (B) is OK after step #1");

    // (3) A computes the shared secret
    in = null;
    try
      {
        in = new IncomingMessage(out.toByteArray());
      }
    catch (KeyAgreementException x)
      {
        harness.debug(x);
        harness.fail("while feeding Client (A), Server's (B) incoming message");
      }
    out = null;
    try
      {
        out = A.processMessage(in);
      }
    catch (KeyAgreementException x)
      {
        harness.debug(x);
        harness.fail("while User (A) is in step #2");
      }
    harness.check(A.isComplete(), "Client (A) is complete after step #2");

    byte[] k1 = null;
    try
      {
        k1 = A.getSharedSecret();
      }
    catch (KeyAgreementException x)
      {
        harness.fail("while accessing Client's (A) version of the shared secret");
      }

    // (4) B computes the shared secret
    in = null;
    try
      {
        in = new IncomingMessage(out.toByteArray());
      }
    catch (KeyAgreementException x)
      {
        harness.debug(x);
        harness.fail("while feeding Server (B), Client's (A) incoming message");
      }
    try
      {
        B.processMessage(in);
      }
    catch (KeyAgreementException x)
      {
        harness.debug(x);
        harness.fail("while Server (B) is in step #2");
      }
    harness.check(B.isComplete(), "Server (B) is complete after step #2");

    byte[] k2 = null;
    try
      {
        k2 = B.getSharedSecret();
      }
    catch (KeyAgreementException x)
      {
        harness.fail("while accessing Host's (B) version of the shared secret");
      }

    harness.check(Arrays.equals(k1, k2),
                  "Client (A) and Server (B) share the same secret");

    tearDown();
  }

  private void setUp() throws IOException
  {
    final Map context = new HashMap();
    context.put(SRPRegistry.PASSWORD_FILE, pFile);
    tpasswd.activate(context);

    Map credentials;
    final Map userID = new HashMap();
    userID.put(Registry.SASL_USERNAME, I);
    userID.put(SRPRegistry.MD_NAME_FIELD, Registry.MD5_HASH);
    try
      {
        credentials = tpasswd.lookup(userID);
        // user exists. update its credentials
        userID.put(Registry.SASL_PASSWORD, p);
        userID.put(SRPRegistry.CONFIG_NDX_FIELD,
                   credentials.get(SRPRegistry.CONFIG_NDX_FIELD));

        tpasswd.update(userID);

      }
    catch (AuthenticationException x)
      { // create new user
        userID.put(Registry.SASL_PASSWORD, p);
        final byte[] salt = new byte[10];
        prng.nextBytes(salt);
        userID.put(SRPRegistry.SALT_FIELD, Util.toBase64(salt));
        userID.put(SRPRegistry.CONFIG_NDX_FIELD, SRPRegistry.N_512_BITS);

        tpasswd.update(userID);
      }

    credentials = tpasswd.lookup(userID);

    //      BigInteger s = new BigInteger(1, Util.fromBase64(
    //            (String) credentials.get(SRPRegistry.SALT_FIELD)));
    //      BigInteger v = new BigInteger(1, Util.fromBase64(
    //            (String) credentials.get(SRPRegistry.USER_VERIFIER_FIELD)));

    final String mode = (String) credentials.get(SRPRegistry.CONFIG_NDX_FIELD);
    final Map configuration = tpasswd.getConfiguration(mode);

    N = new BigInteger(
        1,
        Util.fromBase64((String) configuration.get(SRPRegistry.SHARED_MODULUS)));
    g = new BigInteger(
        1,
        Util.fromBase64((String) configuration.get(SRPRegistry.FIELD_GENERATOR)));
  }

  private void tearDown()
  {
    try
      {
        new File(pFile).delete(); // remove test file
      }
    catch (Exception ignored)
      {
      }

    try
      {
        new File(p2File).delete(); // remove test2 file
      }
    catch (Exception ignored)
      {
      }

    try
      {
        new File(cFile).delete(); // remove test.conf file
      }
    catch (Exception ignored)
      {
      }
  }
}