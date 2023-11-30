/* TestOfKhazad.java -- 
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
// Uses: BaseCipherTestCase

package gnu.testlet.gnu.javax.crypto.cipher;

import gnu.javax.crypto.cipher.IBlockCipher;
import gnu.javax.crypto.cipher.Khazad;
import gnu.testlet.TestHarness;
import java.util.HashMap;

/**
 * Conformance tests for the {@link Khazad} implementation.
 */
public class TestOfKhazad extends BaseCipherTestCase
{
  // KAT and MCT vectors used in this test case
  private static final String[] vk_128;

  private static final String[] vt_128;

  private static final String[] mct_ecb_e_128;

  private static final String[] mct_ecb_d_128;

  private static final String[] mct_cbc_e_128;

  private static final String[] mct_cbc_d_128;

  // static initialiser
  static
    {
      vk_128 = new String[] { "49A4CE32AC190E3F", "BD2226C1128B4AD1",
                             "A3C8D3CAB9D196BC", "2C8146E405C2EA36",
                             "9EC02CFC7065D8F8" };

      vt_128 = new String[] { "9E399864F78ECA02", "3EABB25778098FF7",
                             "A359C027CB02BC47", "36E62B8D8DDF2929",
                             "CB4204ACEDDFE80E" };

      mct_ecb_e_128 = new String[] { "1C8ABEB5F5D8337C", "D29DDD7B07AA2E2E",
                                    "2DCA0196F9AF94DA", "100AFC93082BC492",
                                    "7C4EB4E12D5310BA" };

      mct_ecb_d_128 = new String[] { "0EF3A83A8A874A5A", "BB83871935B33F01",
                                    "ED25D06041BB09CF", "A4091D256FFAC8B6",
                                    "DAC274A3D13600F8" };

      mct_cbc_e_128 = new String[] { "AB983C213749B3CA", "9B0C44EF8B2EA836",
                                    "748AFB0A891F1556", "C7012DE469A78E5D",
                                    "DB95DB1BD214C348" };

      mct_cbc_d_128 = new String[] { "DE93205588933B11", "8651C2BC76A096F6",
                                    "4C9494F2BA8C55CF", "44EE0CB0AA12B9EC",
                                    "6D759B4000216139" };
    }

  public void test(TestHarness harness)
  {
    harness.checkPoint("TestOfKhazad");
    cipher = new Khazad();
    HashMap attrib = new HashMap();
    attrib.put(IBlockCipher.CIPHER_BLOCK_SIZE, new Integer(8));
    attrib.put(IBlockCipher.KEY_MATERIAL, new byte[16]);
    try
      {
        cipher.init(attrib);
        String algorithm = cipher.name();
        harness.check(validityTest(), "validityTest(" + algorithm + ")");
        harness.check(cloneabilityTest(), "cloneabilityTest(" + algorithm + ")");
        harness.check(katVK(vk_128, cipher, 16), "KAT VK " + algorithm + "-128");
        harness.check(katVT(vt_128, cipher, 16), "KAT VT " + algorithm + "-128");
        harness.check(mctEncryptECB(mct_ecb_e_128, cipher, 16),
                      "MCT ECB Encryption " + algorithm + "-128");
        harness.check(mctDecryptECB(mct_ecb_d_128, cipher, 16),
                      "MCT ECB Decryption " + algorithm + "-128");
        harness.check(mctEncryptCBC(mct_cbc_e_128, cipher, 16),
                      "MCT CBC Encryption " + algorithm + "-128");
        harness.check(mctDecryptCBC(mct_cbc_d_128, cipher, 16),
                      "MCT CBC Decryption " + algorithm + "-128");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfKhazad");
      }
  }
}