/* TestOfKeystore.java
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

package gnu.testlet.gnu.javax.crypto.jce.keyring;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.math.BigInteger;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.cert.Certificate;
import java.security.cert.CertificateFactory;
import java.util.Arrays;

import gnu.java.security.Registry;
import gnu.java.security.key.dss.DSSPrivateKey;
import gnu.java.security.key.dss.DSSPublicKey;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Conformance tests for the GNU GKR KeyStore Adapter classes.
 */
public class TestOfKeystore
    implements Testlet
{
  /** KeyStore password. */
  private static final char[] STORE_PASSWORD = "secret".toCharArray();
  /** Private Key Entry alias. */
  private static final String ALIAS = "mykey";
  /** Public Key Entry alias. */
  private static final String ALIAS_DSA = "mykey_dsa";
  /** Trusted Certificate Entry alias. */
  private static final String ANOTHER_ALIAS = "herkey";
  /** Key Entry password. */
  private static final char[] KEY_PASSWORD = "changeme".toCharArray();
  /** Common DSA key material - p. */
  private static final BigInteger p = new BigInteger(
      "fd7f53811d75122952df4a9c2eece4e7f611b7523cef4400c31e3f80b6512669455d4"
      + "02251fb593d8d58fabfc5f5ba30f6cb9b556cd7813b801d346ff26660b76b9950a5"
      + "a49f9fe8047b1022c24fbba9d7feb7c61bf83b57e7c6a8a6150f04fb83f6d3c51ec"
      + "3023554135a169132f675f3ae2b61d72aeff22203199dd14801c7", 16);
  /** Common DSA key material - q. */
  private static final BigInteger q = new BigInteger(
      "9760508f15230bccb292b982a2eb840bf0581cf5", 16);
  /** Common DSA key material - g. */
  private static final BigInteger g = new BigInteger(
      "f7e1a085d69b3ddecbbcab5c36b857b97994afbbfa3aea82f9574c0b3d07826751595"
      + "78ebad4594fe67107108180b449167123e84c281613b7cf09328cc8a6e13c167a8b"
      + "547c8d28e0a3ae1e2bb3a675916ea37f0bfa213562f1fb627a01243bcca4f1bea85"
      + "19089a883dfe15ae59f06928b665e807b552564014c3bfecf492a", 16);
  /** Public DSA key part. */
  private static final BigInteger x = new BigInteger(
      "631305a19984821b95a8c776d38167a4ea2ceb8", 16);
  /** Private DSA key part. */
  private static final BigInteger y = new BigInteger(
      "cc1045a7550205a581ec3a9fed50c6d4aaae9ef2512c066f0d52617e0d462895c00bd"
      + "f2d329c53a9c0f690e406d49e21beb557d47436df9cdda5ad2f532620a5260704c5"
      + "91920ff674666e2166066727051f3d515aedf03a4bdb2d69dd13bbd9b5e7941ff37"
      + "fb35f2d9138b4172e64393b04afdcc630739fbe6993f27f467e17", 16);
  /** Alias's Self-signed certificate. */
  private static final String SELF_SIGNED_CERT = "\n"
      + "-----BEGIN CERTIFICATE-----\n"
      + "MIIC9zCCAregAwIBAAIBATAJBgcqhkjOOAQDMGIxFzAVBgNVBAMMDlJhaWYgUy4g\n"
      + "TmFmZmFoMRswGQYDVQQKDBJUaGUgU2FtcGxlIENvbXBhbnkxDzANBgNVBAcMBlN5\n"
      + "ZG5leTEMMAoGA1UECAwDTlNXMQswCQYDVQQGDAJBVTAeFw0wNjA1MTgwOTM5Mzla\n"
      + "Fw0wNjA4MTYwOTM5MzlaMGIxFzAVBgNVBAMMDlJhaWYgUy4gTmFmZmFoMRswGQYD\n"
      + "VQQKDBJUaGUgU2FtcGxlIENvbXBhbnkxDzANBgNVBAcMBlN5ZG5leTEMMAoGA1UE\n"
      + "CAwDTlNXMQswCQYDVQQGDAJBVTCCAbgwggEsBgcqhkjOOAQBMIIBHwKBgQD9f1OB\n"
      + "HXUSKVLfSpwu7OTn9hG3UjzvRADDHj+AtlEmaUVdQCJR+1k9jVj6v8X1ujD2y5tV\n"
      + "bNeBO4AdNG/yZmC3a5lQpaSfn+gEexAiwk+7qdf+t8Yb+DtX58aophUPBPuD9tPF\n"
      + "HsMCNVQTWhaRMvZ1864rYdcq7/IiAxmd0UgBxwIVAJdgUI8VIwvMspK5gqLrhAvw\n"
      + "WBz1AoGBAPfhoIXWmz3ey7yrXDa4V7l5lK+7+jrqgvlXTAs9B4JnUVlXjrrUWU/m\n"
      + "cQcQgYC0SRZxI+hMKBYTt88JMozIpuE8FnqLVHyNKOCjrh4rs6Z1kW6jfwv6ITVi\n"
      + "8ftiegEkO8yk8b6oUZCJqIPf4VrlnwaSi2ZegHtVJWQBTDv+z0kqA4GFAAKBgQDM\n"
      + "EEWnVQIFpYHsOp/tUMbUqq6e8lEsBm8NUmF+DUYolcAL3y0ynFOpwPaQ5AbUniG+\n"
      + "tVfUdDbfnN2lrS9TJiClJgcExZGSD/Z0Zm4hZgZnJwUfPVFa7fA6S9stad0Tu9m1\n"
      + "55Qf83+zXy2ROLQXLmQ5OwSv3MYwc5++aZPyf0Z+FzAJBgcqhkjOOAQDAy8AMCwC\n"
      + "FE5UuDO5crsK+kOwdqzbr0WCzQQrAhR7ZwA+cBYvIfP3kr7lItJhY4nRBQ==\n"
      + "-----END CERTIFICATE-----\n";
  private static final String CA_SIGNED_CERT = "\n"
      + "-----BEGIN CERTIFICATE-----\n"
      + "MIIDHTCCAsegAwIBAgIKY1JbPwAAAAAqpTANBgkqhkiG9w0BAQUFADBgMQswCQYD\n"
      + "VQQGEwJHQjERMA8GA1UEChMIQXNjZXJ0aWExJjAkBgNVBAsTHUNsYXNzIDEgQ2Vy\n"
      + "dGlmaWNhdGUgQXV0aG9yaXR5MRYwFAYDVQQDEw1Bc2NlcnRpYSBDQSAxMB4XDTA2\n"
      + "MDQyNzAwNDk1NloXDTA3MDQyNzAwNTk1NlowZjELMAkGA1UEBhMCQVUxDDAKBgNV\n"
      + "BAgTA05TVzEPMA0GA1UEBxMGU3lkbmV5MRswGQYDVQQKExJUaGUgU2FtcGxlIENv\n"
      + "bXBhbnkxGzAZBgNVBAMTEnNvbG9tb24udG5nLmNvbS5hdTCBnzANBgkqhkiG9w0B\n"
      + "AQEFAAOBjQAwgYkCgYEArqzuAz6OY5GuyT+U0k45iYMKBmTFRJ0WcLeCpCYIkDuW\n"
      + "wxoRpVdLeoHz5sz5wbj/BLqk6MUgG8kbCILGi5SntpjgmNEynAYoLsh5rrGp7/P0\n"
      + "CiymIgFmvVynR4+tQ40/EbBGpO7feSkhPVTnmk5oX7VW1KCDDfJxvqr5/3u6m9UC\n"
      + "AwEAAaOCARcwggETMB0GA1UdDgQWBBQdKwf6zaazew9E+c77pAvSzNY8nTBjBgNV\n"
      + "HSMEXDBagBSU/lmHRXvTSToKiu9ExYH2J9WQGaE/pD0wOzELMAkGA1UEBhMCR0Ix\n" 
      + "ETAPBgNVBAoTCEFzY2VydGlhMRkwFwYDVQQDExBBc2NlcnRpYSBSb290IENBggEN\n"
      + "ME0GA1UdHwRGMEQwQqBAoD6GPGh0dHA6Ly93d3cuYXNjZXJ0aWEuY29tL09ubGlu\n"
      + "ZUNBL2NybHMvQXNjZXJ0aWFDQTEvY2xhc3MxLmNybDA+BggrBgEFBQcBAQQyMDAw\n"
      + "LgYIKwYBBQUHMAKGImh0dHA6Ly9vY3NwLmdsb2JhbHRydXN0ZmluZGVyLmNvbS8w\n"
      + "DQYJKoZIhvcNAQEFBQADQQCHVsnDcYyjikSMmc3++UVj9XpFkJZqaI4U6baoP68Q\n"
      + "hPe5niht+x2ez35j+Ytx7aNu0MVcAiyA6eyTE9ZYP/vU\n"
      + "-----END CERTIFICATE-----\n";

  /*
   * (non-Javadoc)
   * 
   * @see gnu.testlet.Testlet#test(gnu.testlet.TestHarness)
   */
  public void test(TestHarness harness)
  {
    harness.checkPoint("TestOfKeystore");
    try
      {
        // instantiate the key-store
        KeyStore ks1 = KeyStore.getInstance("gkr");
        harness.check(ks1 != null, "GKR-type KeyStore MUST be available");
        // load it with null i/o stream; i.e. create it
        ks1.load(null, STORE_PASSWORD);
        // store a (private) key-entry
        PrivateKey pk1 = new DSSPrivateKey(Registry.ASN1_ENCODING_ID, p, q, g, x);
        CertificateFactory x509Factory = CertificateFactory.getInstance("X.509");
        byte[] certificateBytes = SELF_SIGNED_CERT.getBytes("ASCII");
        ByteArrayInputStream bais = new ByteArrayInputStream(certificateBytes);
        Certificate c1 = x509Factory.generateCertificate(bais);
        Certificate[] chain1 = new Certificate[] { c1 };
        ks1.setKeyEntry(ALIAS, pk1, KEY_PASSWORD, chain1);
        // retrieve the same (private) key-entry
        PrivateKey pk2 = (PrivateKey) ks1.getKey(ALIAS, KEY_PASSWORD);
        // check that private key is the same
        harness.check(pk2, pk1, "In-memory and original private key MUST be equal");
        // retrieve the certificate
        Certificate[] chain2 = ks1.getCertificateChain(ALIAS);
        harness.check(Arrays.equals(chain2, chain1),
                      "In-memory and original certificate MUST be equal");
        // store a (public) key entry
        PublicKey k1 = new DSSPublicKey(Registry.ASN1_ENCODING_ID, p, q, g, y);
        ks1.setKeyEntry(ALIAS_DSA, k1, null, null);
        // retrieve the same (public) key-entry
        PublicKey k2 = (PublicKey) ks1.getKey(ALIAS_DSA, null);
        // check it's still the same
        harness.check(k2, k1, "In-memory and original public key MUST be equal");
        // see if alias was used for a trusted certificate
        Certificate cert = ks1.getCertificate(ALIAS);
        harness.check(cert == null,
                      "Trusted certificate w/ same Alias MUST NOT be found");
        // must not be able to use same alias for a trusted certificate
        certificateBytes = CA_SIGNED_CERT.getBytes("ASCII");
        bais = new ByteArrayInputStream(certificateBytes);
        Certificate tc1 = x509Factory.generateCertificate(bais);
        String msg = "MUST NOT be able to use same alias for a trusted certificate";
        try
          {
            ks1.setCertificateEntry(ALIAS, tc1);
            harness.fail(msg);
          }
        catch (KeyStoreException e)
          {
            harness.check(true, msg);
          }
        // store a trusted certificate
        ks1.setCertificateEntry(ANOTHER_ALIAS, tc1);
        // retrieve the same trusted certificate
        Certificate tc2 = ks1.getCertificate(ANOTHER_ALIAS);
        // check it's still the same
        harness.check(tc2, tc1,
                      "In-memory and original trusted certificate MUST be equal");

        // persist the key-store
        File ksFile = File.createTempFile("gkr-", ".gks");
        ksFile.deleteOnExit();
        FileOutputStream fos = new FileOutputStream(ksFile);
        ks1.store(fos, STORE_PASSWORD);
        fos.flush();
        fos.close();
        // re-load it from the persisted file
        KeyStore ks2 = KeyStore.getInstance("gkr");
        FileInputStream fis = new FileInputStream(ksFile);
        ks2.load(fis, STORE_PASSWORD);
        // retrieve the private key
        PrivateKey pk3 = (PrivateKey) ks2.getKey(ALIAS, KEY_PASSWORD);
        // check that private key is the same
        harness.check(pk3, pk1, "Persisted and original private key MUST be equal");
        // retrieve the certificate
        Certificate[] chain3 = ks2.getCertificateChain(ALIAS);
        // check that it's still the same
        harness.check(Arrays.equals(chain3, chain1),
                      "Persisted and original certificate MUST be equal");
        // retrieve the public key
        PublicKey k3 = (PublicKey) ks2.getKey(ALIAS_DSA, null);
        // check it's still the same
        harness.check(k3, k1, "Persisted and original public key MUST be equal");
        // retrieve the trusted certificate
        Certificate tc3 = ks2.getCertificate(ANOTHER_ALIAS);
        // check it's still the same
        harness.check(tc3, tc1,
                      "Persisted and original trusted certificate MUST be equal");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfKeystore");
      }
  }
}
