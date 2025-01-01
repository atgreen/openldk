/* TestOfPR28678.java
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

package gnu.testlet.gnu.java.security;

import gnu.javax.security.auth.callback.ConsoleCallbackHandler;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.security.AlgorithmParameterGenerator;
import java.security.AlgorithmParameters;
import java.security.KeyFactory;
import java.security.KeyPairGenerator;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.Provider;
import java.security.SecureRandom;
import java.security.Security;
import java.security.Signature;
import java.security.cert.CertPathBuilder;
import java.security.cert.CertPathValidator;
import java.security.cert.CertStore;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;

import javax.crypto.Cipher;
import javax.crypto.ExemptionMechanism;
import javax.crypto.KeyAgreement;
import javax.crypto.KeyGenerator;
import javax.crypto.Mac;
import javax.crypto.SecretKeyFactory;
import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.TrustManagerFactory;

/**
 * Regression test for PR Classpath/28678.
 */
public class TestOfPR28678
    implements Testlet
{
  public void test(TestHarness harness)
  {
    harness.checkPoint("TestOfPR28678");
    try
      {
        Provider p1 = new FakeProvider("P1");
        Security.addProvider(p1);
        Provider p2 = new FakeProvider("P2");
        Security.addProvider(p2);
        Provider p3 = new FakeProvider("P3");
        Security.addProvider(p3);
        Provider p4 = new FakeProvider("P4");
        Security.addProvider(p4);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfPR28678: " + x);
      }

    try
      {
        testCallbackHandler(harness);
        testCipher(harness);
        testExemptionMechanism(harness);
        testKeyAgreement(harness);
        testKeyGenerator(harness);
        testMac(harness);
        testSecretKeyFactory(harness);
        testKeyManagerFactory(harness);
        testSSLContext(harness);
        testTrustManagerFactory(harness);
        testAlgorithmParameterGenerator(harness);
        testAlgorithmParameters(harness);
        testKeyFactory(harness);
        testKeyPairGenerator(harness);
        testKeyStore(harness);
        testMessageDigest(harness);
        testSecureRandom(harness);
        testSignature(harness);
        testCertificateFactory(harness);
        testCertPathBuilder(harness);
        testCertPathValidator(harness);
        testCertStore(harness);
      }
    finally
      {
        Security.removeProvider("P1");
        Security.removeProvider("P2");
        Security.removeProvider("P3");
        Security.removeProvider("P4");
      }
  }

  private void testCallbackHandler(TestHarness harness)
  {
    String msg;
    msg = "MUST NOT be able to instantiate CallbackHandler algorithm from designated provider";
    try
      {
        ConsoleCallbackHandler.getInstance("foo", "P1");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkClassNotFoundException(harness, x);
      }

    msg = "MUST NOT be able to instantiate CallbackHandler algorithm from any provider";
    try
      {
        ConsoleCallbackHandler.getInstance("foo");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkClassNotFoundException(harness, x);
      }
  }

  private void testCipher(TestHarness harness)
  {
    String msg;
    msg = "MUST NOT be able to instantiate Cipher algorithm from designated provider";
    try
      {
        Cipher.getInstance("foo", "P1");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkClassNotFoundException(harness, x);
      }

    msg = "MUST NOT be able to instantiate Cipher algorithm from any provider";
    try
      {
        Cipher.getInstance("foo");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkClassNotFoundException(harness, x);
      }
  }

  private void testExemptionMechanism(TestHarness harness)
  {
    String msg;
    msg = "MUST NOT be able to instantiate ExemptionMechanism algorithm from designated provider";
    try
      {
        ExemptionMechanism.getInstance("foo", "P1");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkClassNotFoundException(harness, x);
      }

    msg = "MUST NOT be able to instantiate ExemptionMechanism algorithm from any provider";
    try
      {
        ExemptionMechanism.getInstance("foo");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkClassNotFoundException(harness, x);
      }
  }

  private void testKeyAgreement(TestHarness harness)
  {
    String msg;
    msg = "MUST NOT be able to instantiate KeyAgreement algorithm from designated provider";
    try
      {
        KeyAgreement.getInstance("foo", "P1");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkClassNotFoundException(harness, x);
      }

    msg = "MUST NOT be able to instantiate KeyAgreement algorithm from any provider";
    try
      {
        KeyAgreement.getInstance("foo");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkClassNotFoundException(harness, x);
      }
  }

  private void testKeyGenerator(TestHarness harness)
  {
    String msg;
    msg = "MUST NOT be able to instantiate KeyGenerator algorithm from designated provider";
    try
      {
        KeyGenerator.getInstance("foo", "P1");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkClassNotFoundException(harness, x);
      }

    msg = "MUST NOT be able to instantiate KeyGenerator algorithm from any provider";
    try
      {
        KeyGenerator.getInstance("foo");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkClassNotFoundException(harness, x);
      }
  }

  private void testMac(TestHarness harness)
  {
    String msg;
    msg = "MUST NOT be able to instantiate Mac algorithm from designated provider";
    try
      {
        Mac.getInstance("foo", "P1");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkClassNotFoundException(harness, x);
      }

    msg = "MUST NOT be able to instantiate Mac algorithm from any provider";
    try
      {
        Mac.getInstance("foo");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkClassNotFoundException(harness, x);
      }
  }

  private void testSecretKeyFactory(TestHarness harness)
  {
    String msg;
    msg = "MUST NOT be able to instantiate SecretKeyFactory algorithm from designated provider";
    try
      {
        SecretKeyFactory.getInstance("foo", "P1");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkClassNotFoundException(harness, x);
      }

    msg = "MUST NOT be able to instantiate SecretKeyFactory algorithm from any provider";
    try
      {
        SecretKeyFactory.getInstance("foo");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkClassNotFoundException(harness, x);
      }
  }

  private void testKeyManagerFactory(TestHarness harness)
  {
    String msg;
    msg = "MUST NOT be able to instantiate KeyManagerFactory algorithm from designated provider";
    try
      {
        KeyManagerFactory.getInstance("foo", "P1");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkClassNotFoundException(harness, x);
      }

    msg = "MUST NOT be able to instantiate KeyManagerFactory algorithm from any provider";
    try
      {
        KeyManagerFactory.getInstance("foo");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkClassNotFoundException(harness, x);
      }
  }

  private void testSSLContext(TestHarness harness)
  {
    String msg;
    msg = "MUST NOT be able to instantiate SSLContext algorithm from designated provider";
    try
      {
        SSLContext.getInstance("foo", "P1");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkClassNotFoundException(harness, x);
      }

    msg = "MUST NOT be able to instantiate SSLContext algorithm from any provider";
    try
      {
        SSLContext.getInstance("foo");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkClassNotFoundException(harness, x);
      }
  }

  private void testTrustManagerFactory(TestHarness harness)
  {
    String msg;
    msg = "MUST NOT be able to instantiate TrustManagerFactory algorithm from designated provider";
    try
      {
        TrustManagerFactory.getInstance("foo", "P1");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkClassNotFoundException(harness, x);
      }

    msg = "MUST NOT be able to instantiate TrustManagerFactory algorithm from any provider";
    try
      {
        TrustManagerFactory.getInstance("foo");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkClassNotFoundException(harness, x);
      }
  }

  private void testAlgorithmParameterGenerator(TestHarness harness)
  {
    String msg;
    msg = "MUST NOT be able to instantiate AlgorithmParameterGenerator algorithm from designated provider";
    try
      {
        AlgorithmParameterGenerator.getInstance("foo", "P1");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkClassNotFoundException(harness, x);
      }

    msg = "MUST NOT be able to instantiate AlgorithmParameterGenerator algorithm from any provider";
    try
      {
        AlgorithmParameterGenerator.getInstance("foo");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkClassNotFoundException(harness, x);
      }
  }

  private void testAlgorithmParameters(TestHarness harness)
  {
    String msg;
    msg = "MUST NOT be able to instantiate AlgorithmParameters algorithm from designated provider";
    try
      {
        AlgorithmParameters.getInstance("foo", "P1");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkClassNotFoundException(harness, x);
      }

    msg = "MUST NOT be able to instantiate AlgorithmParameters algorithm from any provider";
    try
      {
        AlgorithmParameters.getInstance("foo");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkClassNotFoundException(harness, x);
      }
  }

  private void testKeyFactory(TestHarness harness)
  {
    String msg;
    msg = "MUST NOT be able to instantiate KeyFactory algorithm from designated provider";
    try
      {
        KeyFactory.getInstance("foo", "P1");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkClassNotFoundException(harness, x);
      }

    msg = "MUST NOT be able to instantiate KeyFactory algorithm from any provider";
    try
      {
        KeyFactory.getInstance("foo");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkClassNotFoundException(harness, x);
      }
  }

  private void testKeyPairGenerator(TestHarness harness)
  {
    String msg;
    msg = "MUST NOT be able to instantiate KeyPairGenerator algorithm from designated provider";
    try
      {
        KeyPairGenerator.getInstance("foo", "P1");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkClassNotFoundException(harness, x);
      }

    msg = "MUST NOT be able to instantiate KeyPairGenerator algorithm from any provider";
    try
      {
        KeyPairGenerator.getInstance("foo");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkClassNotFoundException(harness, x);
      }
  }

  private void testKeyStore(TestHarness harness)
  {
    String msg;
    msg = "MUST NOT be able to instantiate KeyStore algorithm from designated provider";
    try
      {
        KeyStore.getInstance("foo", "P1");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkKeyStoreException(harness, x);
      }

    msg = "MUST NOT be able to instantiate KeyStore algorithm from any provider";
    try
      {
        KeyStore.getInstance("foo");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkKeyStoreException(harness, x);
      }
  }

  private void testMessageDigest(TestHarness harness)
  {
    String msg;
    msg = "MUST NOT be able to instantiate MessageDigest algorithm from designated provider";
    try
      {
        MessageDigest.getInstance("foo", "P1");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkClassNotFoundException(harness, x);
      }

    msg = "MUST NOT be able to instantiate MessageDigest algorithm from any provider";
    try
      {
        MessageDigest.getInstance("foo");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkClassNotFoundException(harness, x);
      }
  }

  private void testSecureRandom(TestHarness harness)
  {
    String msg;
    msg = "MUST NOT be able to instantiate SecureRandom algorithm from designated provider";
    try
      {
        SecureRandom.getInstance("foo", "P1");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkClassNotFoundException(harness, x);
      }

    msg = "MUST NOT be able to instantiate SecureRandom algorithm from any provider";
    try
      {
        SecureRandom.getInstance("foo");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkClassNotFoundException(harness, x);
      }
  }

  private void testSignature(TestHarness harness)
  {
    String msg;
    msg = "MUST NOT be able to instantiate Signature algorithm from designated provider";
    try
      {
        Signature.getInstance("foo", "P1");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkClassNotFoundException(harness, x);
      }

    msg = "MUST NOT be able to instantiate Signature algorithm from any provider";
    try
      {
        Signature.getInstance("foo");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkClassNotFoundException(harness, x);
      }
  }

  private void testCertificateFactory(TestHarness harness)
  {
    String msg;
    msg = "MUST NOT be able to instantiate CertificateFactory algorithm from designated provider";
    try
      {
        CertificateFactory.getInstance("foo", "P1");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkCertificateException(harness, x);
      }

    msg = "MUST NOT be able to instantiate CertificateFactory algorithm from any provider";
    try
      {
        CertificateFactory.getInstance("foo");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkCertificateException(harness, x);
      }
  }

  private void testCertPathBuilder(TestHarness harness)
  {
    String msg;
    msg = "MUST NOT be able to instantiate CertPathBuilder algorithm from designated provider";
    try
      {
        CertPathBuilder.getInstance("foo", "P1");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkClassNotFoundException(harness, x);
      }

    msg = "MUST NOT be able to instantiate CertPathBuilder algorithm from any provider";
    try
      {
        CertPathBuilder.getInstance("foo");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkClassNotFoundException(harness, x);
      }
  }

  private void testCertPathValidator(TestHarness harness)
  {
    String msg;
    msg = "MUST NOT be able to instantiate CertPathValidator algorithm from designated provider";
    try
      {
        CertPathValidator.getInstance("foo", "P1");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkClassNotFoundException(harness, x);
      }

    msg = "MUST NOT be able to instantiate CertPathValidator algorithm from any provider";
    try
      {
        CertPathValidator.getInstance("foo");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkClassNotFoundException(harness, x);
      }
  }

  private void testCertStore(TestHarness harness)
  {
    String msg;
    msg = "MUST NOT be able to instantiate CertStore algorithm from designated provider";
    try
      {
        CertStore.getInstance("foo", null, "P1");
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkClassNotFoundException(harness, x);
      }

    msg = "MUST NOT be able to instantiate CertStore algorithm from any provider";
    try
      {
        CertStore.getInstance("foo", null);
        harness.fail(msg);
      }
    catch (Exception x)
      {
        harness.check(true, msg);
        checkClassNotFoundException(harness, x);
      }
  }

  private void checkClassNotFoundException(TestHarness harness, Exception x)
  {
    Throwable t = x.getCause();
    harness.check(t != null, "Exception MUST have a Cause");
    harness.check(t instanceof ClassNotFoundException,
                  "Cause MUST be a ClassNotFoundException");
  }

  private void checkKeyStoreException(TestHarness harness, Exception x)
  {
    harness.check(x instanceof KeyStoreException, "MUST throw a KeyStoreException");
    Throwable t = x.getCause();
    harness.check(t != null, "Exception MUST have a Cause");
    harness.check(t instanceof NoSuchAlgorithmException,
                  "Cause MUST be a NoSuchAlgorithmException");
    t = t.getCause();
    harness.check(t != null, "Exception MUST have a Cause");
    harness.check(t instanceof ClassNotFoundException,
                  "Cause MUST be a ClassNotFoundException");
  }

  private void checkCertificateException(TestHarness harness, Exception x)
  {
    harness.check(x instanceof CertificateException, "MUST throw a CertificateException");
    Throwable t = x.getCause();
    harness.check(t != null, "Exception MUST have a Cause");
    harness.check(t instanceof NoSuchAlgorithmException,
                  "Cause MUST be a NoSuchAlgorithmException");
    t = t.getCause();
    harness.check(t != null, "Exception MUST have a Cause");
    harness.check(t instanceof ClassNotFoundException,
                  "Cause MUST be a ClassNotFoundException");
  }

  class FakeProvider extends Provider
  {
    protected FakeProvider(String providerName)
    {
      super(providerName, 1.0, "Mauve testing security provider");

      put("CallbackHandler.foo", "a.b.c");
      put("Cipher.foo", "a.b.c");
      put("ExemptionMechanism.foo", "a.b.c");
      put("KeyAgreement.foo", "a.b.c");
      put("KeyGenerator.foo", "a.b.c");
      put("Mac.foo", "a.b.c");
      put("SecretKeyFactory.foo", "a.b.c");
      put("KeyManagerFactory.foo", "a.b.c");
      put("SSLContext.foo", "a.b.c");
      put("TrustManagerFactory.foo", "a.b.c");
      put("AlgorithmParameterGenerator.foo", "a.b.c");
      put("AlgorithmParameters.foo", "a.b.c");
      put("KeyFactory.foo", "a.b.c");
      put("KeyPairGenerator.foo", "a.b.c");
      put("KeyStore.foo", "a.b.c");
      put("MessageDigest.foo", "a.b.c");
      put("SecureRandom.foo", "a.b.c");
      put("Signature.foo", "a.b.c");
      put("CertificateFactory.foo", "a.b.c");
      put("CertPathBuilder.foo", "a.b.c");
      put("CertPathValidator.foo", "a.b.c");
      put("CertStore.foo", "a.b.c");
    }
  }
}
