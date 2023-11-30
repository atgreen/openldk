/* TestOfCipherAdapter.java -- FIXME: describe
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

import java.security.Key;
import java.security.SecureRandom;

import javax.crypto.Cipher;
import javax.crypto.KeyGenerator;

/**
 * Regression test for PR Classpath/27849.
 */
public class TestOfPR27849
    implements Testlet
{
  private Key key;
  private Cipher cipher;
  byte[] iv;

  /* (non-Javadoc)
   * @see gnu.testlet.Testlet#test(gnu.testlet.TestHarness)
   */
  public void test(TestHarness harness)
  {
    try
      {
        KeyGenerator keyG = KeyGenerator.getInstance("DESede");
        keyG.init(new SecureRandom());
        key = keyG.generateKey();
        cipher = Cipher.getInstance("DESede/CBC/NoPadding");
        String input = "Does this work ?";

        cipher.init(Cipher.ENCRYPT_MODE, key);
        iv = cipher.getIV();
        harness.check(iv != null, "(Encrypting) cipher.getIV() MUST NOT return null");
        harness.check(iv.length == 8, "IV (encryption) length MUST be 8");
        byte[] plaintext = input.getBytes();
        byte[] ciphertext = cipher.doFinal(plaintext);

        iv = null;
        cipher.init(Cipher.DECRYPT_MODE, key, cipher.getParameters());
        iv = cipher.getIV();
        harness.check(iv != null, "(Decrypting) cipher.getIV() MUST NOT return null");
        harness.check(iv.length == 8, "IV (decryption) length MUST be 8");
        byte[] plaintext2 = cipher.doFinal(ciphertext);
        String recovered = new String(plaintext2);

        harness.check(input.equals(recovered),
                      "Original and recovered texts MUST be equal");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail(String.valueOf(x));
      }
  }
}
