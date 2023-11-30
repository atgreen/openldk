/* TestOfCipherOutputStream.java
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

import gnu.java.security.util.Util;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.Arrays;

import javax.crypto.Cipher;
import javax.crypto.CipherInputStream;
import javax.crypto.CipherOutputStream;
import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;

/**
 * Test for problems reported (and fixed) by Marco Trudel (see thread
 * http://gcc.gnu.org/ml/java/2006-09/msg00105.html).
 */
public class TestOfCipherOutputStream
    implements Testlet
{
  private static final byte[] PLAINTEXT;
  static
  {
    PLAINTEXT = new byte[511];
    for (int i = 0; i < 511; i++)
      PLAINTEXT[i] = (byte) i;
  }
  private static final byte[] CIPHERTEXT = Util.toBytesFromString(
        "0A940BB5416EF045F1C39458C653EA5A07FEEF74E1D5036E900EEE118E949293"
      + "5BE87E2E5B447C944B21C9AF7756C0D803F2C3BDCA826BF082D7CFB035CDB8C1"
      + "D533E59B45A153ED7E5E9C5DFCFD4AAA3EF0B1A5E3059DAB21FCE23A7B61C4CA"
      + "ADDE68F7AD497268D31A0DDD5C74B08F3D2D90DCEF49D32822298B878F815581"
      + "AC26591C0F8BD80EE7C7E3A2D14E2B2276F0DFA4F107BD6303879DAC0E2FD795"
      + "5E18D1FEF61D087EC0A33ED734A7918FE315209ED0E7C94F74A65C99F6EADC1E"
      + "AD393003D3E6BC5268F0D833E0050B78D2001826302BD313C41809FFDA1713E8"
      + "D02A48244ECCDC2379224DBC5470361266A7C7E8345231489751DE073316ADAD"
      + "0A940BB5416EF045F1C39458C653EA5A07FEEF74E1D5036E900EEE118E949293"
      + "5BE87E2E5B447C944B21C9AF7756C0D803F2C3BDCA826BF082D7CFB035CDB8C1"
      + "D533E59B45A153ED7E5E9C5DFCFD4AAA3EF0B1A5E3059DAB21FCE23A7B61C4CA"
      + "ADDE68F7AD497268D31A0DDD5C74B08F3D2D90DCEF49D32822298B878F815581"
      + "AC26591C0F8BD80EE7C7E3A2D14E2B2276F0DFA4F107BD6303879DAC0E2FD795"
      + "5E18D1FEF61D087EC0A33ED734A7918FE315209ED0E7C94F74A65C99F6EADC1E"
      + "AD393003D3E6BC5268F0D833E0050B78D2001826302BD313C41809FFDA1713E8"
      + "D02A48244ECCDC2379224DBC54703612D59D78BE4A3F7494AC0ECEC7FA122305");

  public void test(TestHarness harness)
  {
    byte[] k = new byte[16];
    for (int i = 0; i < k.length; i++)
      k[i] = (byte) i;
    try
      {
        SecretKey key = new SecretKeySpec(k, "AES");
        encryptionTest(harness, key);
        decryptionTest(harness, key);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("test(): " + x);
      }
  }

  private void encryptionTest(TestHarness harness, SecretKey key)
    throws Exception
  {
      ByteArrayInputStream in = new ByteArrayInputStream(PLAINTEXT);
      ByteArrayOutputStream outStream = new ByteArrayOutputStream(1024 + 32);

      Cipher cipher = Cipher.getInstance("AES/ECB/PKCS7");

      cipher.init(Cipher.ENCRYPT_MODE, key);
      CipherOutputStream out = new CipherOutputStream(outStream, cipher);

      byte[] b = new byte[2048];
      int n;
      while ((n = in.read(b)) != -1)
        if (n > 0)
          out.write(b, 0, n);

      out.close();

      byte[] ciphertxt = outStream.toByteArray();
      harness.check(Arrays.equals(ciphertxt, CIPHERTEXT), "Cipher text MUST match");
  }

  private void decryptionTest(TestHarness harness, SecretKey key)
    throws Exception
  {
    ByteArrayInputStream inStream = new ByteArrayInputStream(CIPHERTEXT);
    ByteArrayOutputStream out = new ByteArrayOutputStream(1024 + 32);

    Cipher cipher = Cipher.getInstance("AES/ECB/PKCS7");

    cipher.init(Cipher.DECRYPT_MODE, key);
    CipherInputStream in = new CipherInputStream(inStream, cipher);

    byte[] b = new byte[2048];
    int n;
    while ((n = in.read(b)) != -1)
      if (n > 0)
        out.write(b, 0, n);

    in.close();

    byte[] plaintxt = out.toByteArray();
    harness.check(Arrays.equals(plaintxt, PLAINTEXT), "Plain text MUST match");
  }
}
