/* TestOfRijndael.java -- 
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
import gnu.javax.crypto.cipher.Rijndael;
import gnu.testlet.TestHarness;
import java.util.HashMap;

/**
 * Conformance tests for the {@link Rijndael} implementation.
 */
public class TestOfRijndael extends BaseCipherTestCase
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
      vk_128 = new String[] { "0EDD33D3C621E546455BD8BA1418BEC8",
                             "C0CC0C5DA5BD63ACD44A80774FAD5222",
                             "2F0B4B71BC77851B9CA56D42EB8FF080",
                             "6B1E2FFFE8A114009D8FE22F6DB5F876",
                             "9AA042C315F94CBB97B62202F83358F5" };

      vk_192 = new String[] { "DE885DC87F5A92594082D02CC1E1B42C",
                             "C749194F94673F9DD2AA1932849630C1",
                             "0CEF643313912934D310297B90F56ECC",
                             "C4495D39D4A553B225FBA02A7B1B87E1",
                             "636D10B1A0BCAB541D680A7970ADC830" };

      vk_256 = new String[] { "E35A6DCB19B201A01EBCFA8AA22B5759",
                             "5075C2405B76F22F553488CAE47CE90B",
                             "49DF95D844A0145A7DE01C91793302D3",
                             "E7396D778E940B8418A86120E5F421FE",
                             "05F535C36FCEDE4657BE37F4087DB1EF" };

      vt_128 = new String[] { "3AD78E726C1EC02B7EBFE92B23D9EC34",
                             "45BC707D29E8204D88DFBA2F0B0CAD9B",
                             "161556838018F52805CDBD6202002E3F",
                             "F5569B3AB6A6D11EFDE1BF0A64C6854A",
                             "64E82B50E501FBD7DD4116921159B83E" };

      vt_192 = new String[] { "6CD02513E8D4DC986B4AFE087A60BD0C",
                             "423D2772A0CA56DAABB48D2129062987",
                             "1021F2A8DA70EB2219DC16804445FF98",
                             "C636E35B402577F96974D8804295EBB8",
                             "1566D2E57E8393C19E29F892EA28A9A7" };

      vt_256 = new String[] { "DDC6BF790C15760D8D9AEB6F9A75FD4E",
                             "C7098C217C334D0C9BDF37EA13B0822C",
                             "60F0FB0D4C56A8D4EEFEC5264204042D",
                             "73376FBBF654D0686E0E84001477106B",
                             "2F443B52BA5F0C6EA0602C7C4FD259B6" };

      mct_ecb_e_128 = new String[] { "C34C052CC0DA8D73451AFE5F03BE297F",
                                    "0AC15A9AFBB24D54AD99E987208272E2",
                                    "A3D43BFFA65D0E80092F67A314857870",
                                    "355F697E8B868B65B25A04E18D782AFA",
                                    "ACC863637868E3E068D2FD6E3508454A" };

      mct_ecb_e_192 = new String[] { "F3F6752AE8D7831138F041560631B114",
                                    "77BA00ED5412DFF27C8ED91F3C376172",
                                    "2D92DE893574463412BD7D121A94952F",
                                    "96650F835912F5E748422727802C6CE1",
                                    "5FCCD4B5F125ADC5B85A56DB32283732" };

      mct_ecb_e_256 = new String[] { "8B79EECC93A0EE5DFF30B4EA21636DA4",
                                    "C737317FE0846F132B23C8C2A672CE22",
                                    "E58B82BFBA53C0040DC610C642121168",
                                    "10B296ABB40504995DB71DDA0B7E26FB",
                                    "B7198D8E88BAA25234C18517E99BB70D" };

      mct_ecb_d_128 = new String[] { "44416AC2D1F53C583303917E6BE9EBE0",
                                    "E3FD51123B48A2E2AB1DB29894202222",
                                    "877B88A77AEF04F05546539E17259F53",
                                    "C7A71C1B46261602EB1EE48FDA8155A4",
                                    "6B6AC8E00FAF7E045ECCFC426A137221" };

      mct_ecb_d_192 = new String[] { "48E31E9E256718F29229319C19F15BA4",
                                    "CC01684BE9B29ED01EA7923E7D2380AA",
                                    "8726B4E66D6B8FBAA22D42981A5A40CC",
                                    "83B9A21A0710FDB9C603797613772ED6",
                                    "F15479A2B2C250F7E5C11D333D867CBD" };

      mct_ecb_d_256 = new String[] { "058CCFFDBBCB382D1F6F56585D8A4ADE",
                                    "15173A0EB65F5CC05E704EFE61D9E346",
                                    "85F083ACC676D91EDD1ABFB43935237A",
                                    "42C8F0ABC58E0BEAC32911D2DD9FA8C8",
                                    "5E44123D2CA07981B073BB2749F557D6" };

      mct_cbc_e_128 = new String[] { "8A05FC5E095AF4848A08D328D3688E3D",
                                    "192D9B3AA10BB2F7846CCBA0085C657A",
                                    "40D8DAF6D1FDA0A073B3BD18B7695D2E",
                                    "3EDBE80D69A1D2248CA55FC17C4EF3C5",
                                    "D87891CF573C99EAE4349A70CA099415" };

      mct_cbc_e_192 = new String[] { "7BD966D53AD8C1BB85D2ADFAE87BB104",
                                    "869C061BE9CFEAB5D285B0724A9A8970",
                                    "9E58A52B3840DBE16E8063A18220FEE4",
                                    "730A256C202B9D57F3C0D73AD4B6CBED",
                                    "E79EF11C5C1FD1AB75280BCFFCFE89D4" };

      mct_cbc_e_256 = new String[] { "FE3C53653E2F45B56FCD88B2CC898FF0",
                                    "7CE2ABAF8BEF23C4816DC8CE842048A7",
                                    "50CD14A12C6852D39654C816BFAF9AC2",
                                    "3F411DAD0E339FE281637133BF46BD13",
                                    "5BA2C7663A4061719A7CCC2AF2A3EE8A" };

      mct_cbc_d_128 = new String[] { "FACA37E0B0C85373DF706E73F7C9AF86",
                                    "F5372F9735C5685F1DA362AF6ECB2940",
                                    "5496A4C29C7670F61B5D5DF6181F5947",
                                    "940CC5A2AF4F1F8D1862B47BCF63E4CA",
                                    "08832415D97820DE305A58A9AD111A9E" };

      mct_cbc_d_192 = new String[] { "5DF678DD17BA4E75B61768C6ADEF7C7B",
                                    "F9604074F8FA45AC71959888DD056F9F",
                                    "98A957EA6DBE623B7E08F919812A3898",
                                    "AD6D29D6482764BB4BC27A87AE5CD877",
                                    "DA5EB591FDC48F0D9E4EBD373E5717A3" };

      mct_cbc_d_256 = new String[] { "4804E1818FE6297519A3E88C57310413",
                                    "D36C27EBB8FA0BC9FA368DF850FD45FB",
                                    "EBCB4DC84155682856D94B442BC597EE",
                                    "23AA6A6B4BE8C04E19707CA330804C4E",
                                    "9B1AA0F33416484BA68740E821F95CD3" };
    }

  public void test(TestHarness harness)
  {
    harness.checkPoint("TestOfRijndael");
    cipher = new Rijndael();
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
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfRijndael");
      }
  }
}