/* TestOfDHKeyAgreement2.java
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

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.math.BigInteger;
import java.security.AlgorithmParameters;
import java.security.KeyFactory;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.PublicKey;
import java.security.spec.X509EncodedKeySpec;
import java.util.Arrays;

import javax.crypto.Cipher;
import javax.crypto.KeyAgreement;
import javax.crypto.SecretKey;
import javax.crypto.ShortBufferException;
import javax.crypto.interfaces.DHPublicKey;
import javax.crypto.spec.DHParameterSpec;

/**
 * Conformance test, primarily for the Diffie-Hellman key exchange, but covers
 * other areas of the GNU JCE adapters classes.
 */
public class TestOfDHKeyAgreement2
    implements Testlet
{
  // 1024-bit Diffie-Hellman modulus value used in SKIP
  private static final byte skip1024ModulusBytes[] = {
      (byte) 0xF4, (byte) 0x88, (byte) 0xFD, (byte) 0x58,
      (byte) 0x4E, (byte) 0x49, (byte) 0xDB, (byte) 0xCD,
      (byte) 0x20, (byte) 0xB4, (byte) 0x9D, (byte) 0xE4,
      (byte) 0x91, (byte) 0x07, (byte) 0x36, (byte) 0x6B,
      (byte) 0x33, (byte) 0x6C, (byte) 0x38, (byte) 0x0D,
      (byte) 0x45, (byte) 0x1D, (byte) 0x0F, (byte) 0x7C,
      (byte) 0x88, (byte) 0xB3, (byte) 0x1C, (byte) 0x7C,
      (byte) 0x5B, (byte) 0x2D, (byte) 0x8E, (byte) 0xF6,
      (byte) 0xF3, (byte) 0xC9, (byte) 0x23, (byte) 0xC0,
      (byte) 0x43, (byte) 0xF0, (byte) 0xA5, (byte) 0x5B,
      (byte) 0x18, (byte) 0x8D, (byte) 0x8E, (byte) 0xBB,
      (byte) 0x55, (byte) 0x8C, (byte) 0xB8, (byte) 0x5D,
      (byte) 0x38, (byte) 0xD3, (byte) 0x34, (byte) 0xFD,
      (byte) 0x7C, (byte) 0x17, (byte) 0x57, (byte) 0x43,
      (byte) 0xA3, (byte) 0x1D, (byte) 0x18, (byte) 0x6C,
      (byte) 0xDE, (byte) 0x33, (byte) 0x21, (byte) 0x2C,
      (byte) 0xB5, (byte) 0x2A, (byte) 0xFF, (byte) 0x3C,
      (byte) 0xE1, (byte) 0xB1, (byte) 0x29, (byte) 0x40,
      (byte) 0x18, (byte) 0x11, (byte) 0x8D, (byte) 0x7C,
      (byte) 0x84, (byte) 0xA7, (byte) 0x0A, (byte) 0x72,
      (byte) 0xD6, (byte) 0x86, (byte) 0xC4, (byte) 0x03,
      (byte) 0x19, (byte) 0xC8, (byte) 0x07, (byte) 0x29,
      (byte) 0x7A, (byte) 0xCA, (byte) 0x95, (byte) 0x0C,
      (byte) 0xD9, (byte) 0x96, (byte) 0x9F, (byte) 0xAB,
      (byte) 0xD0, (byte) 0x0A, (byte) 0x50, (byte) 0x9B,
      (byte) 0x02, (byte) 0x46, (byte) 0xD3, (byte) 0x08,
      (byte) 0x3D, (byte) 0x66, (byte) 0xA4, (byte) 0x5D,
      (byte) 0x41, (byte) 0x9F, (byte) 0x9C, (byte) 0x7C,
      (byte) 0xBD, (byte) 0x89, (byte) 0x4B, (byte) 0x22,
      (byte) 0x19, (byte) 0x26, (byte) 0xBA, (byte) 0xAB,
      (byte) 0xA2, (byte) 0x5E, (byte) 0xC3, (byte) 0x55,
      (byte) 0xE9, (byte) 0x2F, (byte) 0x78, (byte) 0xC7 };
  private static final BigInteger skip1024Modulus =
      new BigInteger(1, skip1024ModulusBytes);
  private static final BigInteger skip1024Base = BigInteger.valueOf(2L);
  private static final String CLEAR_TEXT = "This is just another example";

  /* (non-Javadoc)
   * @see gnu.testlet.Testlet#test(gnu.testlet.TestHarness)
   */
  public void test(TestHarness harness)
  {
    try
      {
        // Alice side
        DHParameterSpec spec1 = new DHParameterSpec(skip1024Modulus, skip1024Base);
        harness.verbose("*** Generating Alice's Diffie-Hellman key-pair");
        KeyPairGenerator aliceKPG = KeyPairGenerator.getInstance("DH");
        aliceKPG.initialize(spec1);
        KeyPair aliceKP = aliceKPG.generateKeyPair();

        harness.verbose("*** Initializing Alice's Diffie-Hellman key-agreement");
        KeyAgreement aliceKA = KeyAgreement.getInstance("DH");
        aliceKA.init(aliceKP.getPrivate());

        harness.verbose("*** Alice sends Bob her encoded key...");
        byte[] alicePubKEnc = aliceKP.getPublic().getEncoded();

        // Bob side
        KeyFactory bobKF = KeyFactory.getInstance("DH");
        X509EncodedKeySpec x509KeySpec = new X509EncodedKeySpec(alicePubKEnc);
        PublicKey alicePubK = bobKF.generatePublic(x509KeySpec);

        // Bob gets the DH parameters associated with Alice's public key.
        // He MUST use the same parameters when generating his own key-pair
        DHParameterSpec spec2 = ((DHPublicKey) alicePubK).getParams();
        harness.verbose("*** Generating Bob's Diffie-Hellman key-pair");
        KeyPairGenerator bobKPG = KeyPairGenerator.getInstance("DH");
        bobKPG.initialize(spec2);
        KeyPair bobKP = bobKPG.generateKeyPair();

        harness.verbose("*** Initializing Bob's Diffie-Hellman key-agreement");
        KeyAgreement bobKA = KeyAgreement.getInstance("DH");
        bobKA.init(bobKP.getPrivate());

        harness.verbose("*** Bob sends Alice his encoded key...");
        byte[] bobPubKEnc = bobKP.getPublic().getEncoded();

        // Alice uses Bob's public key for the one and only phase of her
        // version of the DH protocol.  Before she can do that, she has to
        // re-generate Bob's DH public key from his encoded key material
        KeyFactory aliceKF = KeyFactory.getInstance("DH");
        x509KeySpec = new X509EncodedKeySpec(bobPubKEnc);
        PublicKey bobPubK = aliceKF.generatePublic(x509KeySpec);
        harness.verbose("*** Alice does phase #1");
        aliceKA.doPhase(bobPubK, true);

        // Bob uses Alice's public key for the first and only phase of his
        // version of the DH protocol
        harness.verbose("*** Bob does phase #1");
        bobKA.doPhase(alicePubK, true);

        // Both generate (hopefully) the same shared secret
        byte[] aliceSS = aliceKA.generateSecret();
        int len = aliceSS.length;

        byte[] bobSS = new byte[len];
        String msg = "generateSecret(byte[" + bobSS.length
                     + "], 1) MUST throw ShortBufferException";
        try // with a shorter buffer first!
          {
            bobKA.generateSecret(bobSS, 1);
            harness.fail(msg);
          }
        catch (ShortBufferException e)
          {
            harness.check(true, msg);
          }
        // now do it properly
        bobKA.generateSecret(bobSS, 0);

        harness.check(Arrays.equals(aliceSS, bobSS),
                      "Shared secrets MUST be equal");

        harness.verbose("*** Bob generates a new shared secret");
        // The call to bobKA.generateSecret above resets the key-agreement
        // object, so we call doPhase() again before calling generateSecret()
        bobKA.doPhase(alicePubK, true);
        SecretKey bobSK = bobKA.generateSecret("DES");

        harness.verbose("*** Alice generates a new shared secret");
        aliceKA.doPhase(bobPubK, true);
        SecretKey aliceSK = aliceKA.generateSecret("DES");

        harness.verbose("*** Bob sends Alice a DES/ECB encrypted message");
        Cipher bobCipher = Cipher.getInstance("DES/ECB/PKCS5Padding");
        bobCipher.init(Cipher.ENCRYPT_MODE, bobSK);

        byte[] cleartext = CLEAR_TEXT.getBytes();
        byte[] ciphertext = bobCipher.doFinal(cleartext);

        // Alice decrypts, using DES in ECB mode
        Cipher aliceCipher = Cipher.getInstance("DES/ECB/PKCS5Padding");
        aliceCipher.init(Cipher.DECRYPT_MODE, aliceSK);
        byte[] recovered = aliceCipher.doFinal(ciphertext);

        harness.check(Arrays.equals(cleartext, recovered),
                      "DES/ECB recovered text and cleartext MUST be equal");

        harness.verbose("*** Bob sends Alice a DES/CBC encrypted message");
        bobCipher = Cipher.getInstance("DES/CBC/PKCS5Padding");
        bobCipher.init(Cipher.ENCRYPT_MODE, bobSK);

        cleartext = CLEAR_TEXT.getBytes();
        ciphertext = bobCipher.doFinal(cleartext);
        // Retrieve the parameter that was used, and transfer it to Alice
        byte[] encodedParams = bobCipher.getParameters().getEncoded();

        // Alice decrypts, using DES in CBC mode
        AlgorithmParameters params = AlgorithmParameters.getInstance("DES");
        params.init(encodedParams);
        aliceCipher = Cipher.getInstance("DES/CBC/PKCS5Padding");
        aliceCipher.init(Cipher.DECRYPT_MODE, aliceSK, params);
        recovered = aliceCipher.doFinal(ciphertext);

        harness.check(Arrays.equals(cleartext, recovered),
                      "DES/CBC recovered text and cleartext MUST be equal");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail(x.getMessage());
      }
  }
}
