/* TestOfSerpent.java -- 
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
import gnu.javax.crypto.cipher.Serpent;
import gnu.testlet.TestHarness;
import java.util.HashMap;

/**
 * Conformance tests for the {@link Serpent} implementation.
 */
public class TestOfSerpent extends BaseCipherTestCase
{
  // KAT and MCT vectors used in this test case
  private static final String[] vk_128;

  private static final String[] vk_192;

  private static final String[] vk_256;

  private static final String[] vt_128;

  private static final String[] vt_192;

  private static final String[] vt_256;

  private static final String[] mct_ecb_e_128;

  private static final String[] mct_ecb_e_192;

  private static final String[] mct_ecb_e_256;

  private static final String[] mct_ecb_d_128;

  private static final String[] mct_ecb_d_192;

  private static final String[] mct_ecb_d_256;

  private static final String[] mct_cbc_e_128;

  private static final String[] mct_cbc_e_192;

  private static final String[] mct_cbc_e_256;

  private static final String[] mct_cbc_d_128;

  private static final String[] mct_cbc_d_192;

  private static final String[] mct_cbc_d_256;

  // static initialiser
  static
    {
      vk_128 = new String[] { "49AFBFAD9D5A34052CD8FFA5986BD2DD",
                             "0C1E2E4E79BD02C2501096E79B5E73FA",
                             "D769E71B31F4AE12E14CDC48238D2D7B",
                             "BD0276D6BE072CC00B3D426130BF355F",
                             "C191E2121DAEA18EC30957BBCE199A8C" };

      vk_192 = new String[] { "E78E5402C7195568AC3678F7A3F60C66",
                             "23645A0DDD4B0F8B73B6215EA938A59E",
                             "95D262643C94CAB3E5830FC90A3AD119",
                             "2A66AE878814C427CD1BE1B929D69D1B",
                             "C1F74B3C5DF4B485118B6901A22BEF14" };

      vk_256 = new String[] { "ABED96E766BF28CBC0EBD21A82EF0819",
                             "959658CDFCD80356DDE045BBE7B1888D",
                             "04D49CC0FAE714B46B5B177664DF4C28",
                             "39F1B1A339DED740D80B663A057D4866",
                             "DF15B01E30CF9C81688F989809579A86" };

      vt_128 = new String[] { "10B5FFB720B8CB9002A1142B0BA2E94A",
                             "91A7847EF1CD87551B5B4BF6F8E96E2C",
                             "5D32AECE8383FB2EE22CB4A6061D1429",
                             "B4895CAD26DFA1538E9AD80599E1E62A",
                             "3B275D40F7DAF4A3F59DDFAB28FF8715" };

      vt_192 = new String[] { "B10B271BA25257E1294F2B51F076D0D9",
                             "D522A3B8D6D89D4D2A124FDD88F36896",
                             "6FAEFEE5F5255D5465C1BEFA672AF1D3",
                             "409E1D63BC71EB0D6F7ECEAA03025897",
                             "8A7E9FEB4300A2A265F4A14E52011BE1" };

      vt_256 = new String[] { "DA5A7992B1B4AE6F8C004BC8A7DE5520",
                             "F351351B823E3D7A4F3BF390C4F198CB",
                             "A477A65D9DB75C8ED7218C52B64C65BB",
                             "F8019452CBA4FE618D80A6756183B2E0",
                             "D43B7B981B829342FCE0E3EC6F5F4C82" };

      mct_ecb_e_128 = new String[] { "90E7A5BA9497FA1BFC00F7D1A3A86A1E",
                                    "5D0C5DA998AAA940D493738892579447",
                                    "B5E6510FBBD63D828ADE0B89AE48EF5F",
                                    "8056B61DACB4D3F52976EF5B1D4165E8",
                                    "3997C4990223E5C70F3CB015F48EC57A" };

      mct_ecb_e_192 = new String[] { "2D8AF7B79EB7F21FDB394C77C3FB8C3A",
                                    "D7585ADA56F93796161C56CDBA61AA3F",
                                    "654C5EB1018D6086717B89DB97E91A5A",
                                    "8F55D4952903B480CE9AE1B2FDCA07F4",
                                    "122E10485449687F99562A5B3CE0C022" };

      mct_ecb_e_256 = new String[] { "92EFA3CA9477794D31F4DF7BCE23E60A",
                                    "41133A29B97E3B4231549E8C2D0AF27E",
                                    "6EE8EDC74DCFEFD0C7BEAEE4CBCBC9C2",
                                    "59DD509F8B303CE5527D20D33BD16697",
                                    "E7C035318D676702FB9BE6802459951A" };

      mct_ecb_d_128 = new String[] { "47C6786045BB9D30F4029E7CCCCD1CAE",
                                    "003380E19F10065740394F48E2FE80B7",
                                    "7A4F7DB38C52A8B711B778A38D203B6B",
                                    "FA57C160B1B826EA22F531DC593DB5B4",
                                    "08E2EA201AB8E452BB4584A09A4633CE" };

      mct_ecb_d_192 = new String[] { "0FB9B00AE4E6E0F328DDC43CEE462898",
                                    "2B088460D9760C2B31F45177036DC67E",
                                    "92348D985879F9CAB7A996D46A691BF1",
                                    "45877877B396CD8DF70259B89D333802",
                                    "5191198DCD3EB19484BD813DC1358697" };

      mct_ecb_d_256 = new String[] { "CFF2F5875D0FB0D3217052FC9D7B94A3",
                                    "BFFB4E6FCCD8345DA6A443852C56CB29",
                                    "060564434A8BFE5AA08A9AABFD4235C1",
                                    "DAA415F8D036A0A8A548A30F072A939F",
                                    "0ED586E6CB70CF5DB8E92145039A344B" };

      mct_cbc_e_128 = new String[] { "9EA101ECEBAA41C712BCB0D9BAB3E2E4",
                                    "F86B2C265B9C75869F31E2C684C13E9F",
                                    "CC1810DDE499DC51461C2B7635288935",
                                    "5772284ED8BB79B1AD054FD6481001E2",
                                    "19BC42A4D5504F5188ABF768D2710C85" };

      mct_cbc_e_192 = new String[] { "71DA83C1C5FBE855469726F8BE27E9D2",
                                    "4613DECB19EC4D226B2A1E894BD7883C",
                                    "34ED7BBC7761A44DBD2B2462D3FA1277",
                                    "E1FADB014F9BC8FDCF76DC280B51E57C",
                                    "97EA261B6F72512A0FD4CE545B698F1E" };

      mct_cbc_e_256 = new String[] { "61558018134F3B22BD2E8F4E5D48FE9A",
                                    "86D2905FA3F2FDCDA12A51106BBF1B77",
                                    "96FA8DAD842B398799AB04A83747D0A4",
                                    "F22567FE517E7345D6ABBDD41FB0FF8B",
                                    "BF17C92289EA278FBA70F128BC36CCD1" };

      mct_cbc_d_128 = new String[] { "0C81512847A5C6E7A1B8C7D15EFA1ACB",
                                    "E5686F847D5F6A5A6BB501CC8B8456A1",
                                    "BD2703C15F749213B83EADD2E3028AE0",
                                    "BE7E1BE63AE8EF1B0AE088809CAC87D4",
                                    "AE65B7EBB75E6BFF3CF653F9E57A53B2" };

      mct_cbc_d_192 = new String[] { "94463805DCE72D0F0379B44F8B418A93",
                                    "722A8B9AAFB559A0C79661CC7AB46629",
                                    "CC8C741DCEB88C076B17268D24593D6F",
                                    "C513F538F32449EB90257E37DF823A20",
                                    "912A0A012B42FE6BE8A9F04AF9D048D2" };

      mct_cbc_d_256 = new String[] { "170E1E83AAC120770660422756C188E6",
                                    "432E4B5D7B619BCD6E9C969B270901DD",
                                    "6BD145CA6C07751335FAD4B144EA2A71",
                                    "8E2F5CE1C20081DA0AE47CA728032EC8",
                                    "3F1D62538956298702A6E5F95DEB25A1" };
    }

  public TestOfSerpent()
  {
    super(LITTLE_ENDIAN);
  }

  public void test(TestHarness harness)
  {
    harness.checkPoint("TestOfSerpent");
    cipher = new Serpent();
    HashMap attrib = new HashMap();
    attrib.put(IBlockCipher.CIPHER_BLOCK_SIZE, new Integer(16));
    attrib.put(IBlockCipher.KEY_MATERIAL, new byte[16]);
    try
      {
        cipher.init(attrib);
        String algorithm = cipher.name();
        harness.check(validityTest(), "validityTest(" + algorithm + ")");
        harness.check(cloneabilityTest(), "cloneabilityTest(" + algorithm + ")");
        harness.check(katVK(vk_128, cipher, 16), "KAT VK " + algorithm + "-128");
        harness.check(katVK(vk_192, cipher, 24), "KAT VK " + algorithm + "-192");
        harness.check(katVK(vk_256, cipher, 32), "KAT VK " + algorithm + "-256");
        harness.check(katVT(vt_128, cipher, 16), "KAT VT " + algorithm + "-128");
        harness.check(katVT(vt_192, cipher, 24), "KAT VT " + algorithm + "-192");
        harness.check(katVT(vt_256, cipher, 32), "KAT VT " + algorithm + "-256");
        harness.check(mctEncryptECB(mct_ecb_e_128, cipher, 16),
                      "MCT ECB Encryption " + algorithm + "-128");
        harness.check(mctEncryptECB(mct_ecb_e_192, cipher, 24),
                      "MCT ECB Encryption " + algorithm + "-192");
        harness.check(mctEncryptECB(mct_ecb_e_256, cipher, 32),
                      "MCT ECB Encryption " + algorithm + "-256");
        harness.check(mctDecryptECB(mct_ecb_d_128, cipher, 16),
                      "MCT ECB Decryption " + algorithm + "-128");
        harness.check(mctDecryptECB(mct_ecb_d_192, cipher, 24),
                      "MCT ECB Decryption " + algorithm + "-192");
        harness.check(mctDecryptECB(mct_ecb_d_256, cipher, 32),
                      "MCT ECB Decryption " + algorithm + "-256");
        harness.check(mctEncryptCBC(mct_cbc_e_128, cipher, 16),
                      "MCT CBC Encryption " + algorithm + "-128");
        harness.check(mctEncryptCBC(mct_cbc_e_192, cipher, 24),
                      "MCT CBC Encryption " + algorithm + "-192");
        harness.check(mctEncryptCBC(mct_cbc_e_256, cipher, 32),
                      "MCT CBC Encryption " + algorithm + "-256");
        harness.check(mctDecryptCBC(mct_cbc_d_128, cipher, 16),
                      "MCT CBC Decryption " + algorithm + "-128");
        harness.check(mctDecryptCBC(mct_cbc_d_192, cipher, 24),
                      "MCT CBC Decryption " + algorithm + "-192");
        harness.check(mctDecryptCBC(mct_cbc_d_256, cipher, 32),
                      "MCT CBC Decryption " + algorithm + "-256");
//        new TestOfNistVectors("serpent", TestOfNistVectors.LITTLE_ENDIAN)
//            .test(harness);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfSerpent");
      }
  }
}