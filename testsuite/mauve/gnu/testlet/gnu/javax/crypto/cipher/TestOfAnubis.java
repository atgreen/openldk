/* TestOfAnubis.java -- 
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

import gnu.javax.crypto.cipher.Anubis;
import gnu.javax.crypto.cipher.IBlockCipher;
import gnu.testlet.TestHarness;
import java.util.HashMap;

/**
 * Conformance tests for the {@link Anubis} implementation.
 */
public class TestOfAnubis extends BaseCipherTestCase
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
      vk_128 = new String[] { "B835BDC334829D8371BFA371E4B3C4FD",
                             "6EEF5FDAC4C6E9914828AE9446A460BB",
                             "3ECACEE372560B9810B5498D5E7791CC",
                             "64F244781EE6DDD181BB5934A7A12F4E",
                             "4F39939A9F45EF5F1F596E7BABA1EC6F" };

      vk_192 = new String[] { "7D623B52C74C64D8EBC72D579785438F",
                             "AC166073FAA7E04A40C88455FF11BAED",
                             "430ECACFD834527AE0E990668AA11B8F",
                             "576566892CB51866C2810FF60A05FEFB",
                             "53554C9CAC6727B76E294BF166F25646" };

      vk_256 = new String[] { "9600F07691692987F5E597DBDBAF1B0A",
                             "1CB51DECD24FDA26926D05AD0F96AA28",
                             "7E1DE6BBC06BB5BE2F6D6A719FE5B807",
                             "99EBAA541999213EEB7AD212B21FA3A2",
                             "2ED5C152EE4D0347F2C2677E0A7A43D5" };

      vt_128 = new String[] { "753843F8182D1A74346EA255242181E0",
                             "741696DC3C9969A64F55DB17F5E4522F",
                             "D7009A0DFDB7EA99BF4B944284C4FFE0",
                             "708EB676B1DAE91CAAC8BFAAF060F1B9",
                             "0BEF93A942AD81430B8E0AE0CC56C30E" };

      vt_192 = new String[] { "CEB51617789006534C2339B90E5CD678",
                             "1EEE27469C93609DE05C1636549E485D",
                             "C242A63345C449ADAEC0CD164E00C22E",
                             "85BD1C84396110F91D4DFB426FA2E95A",
                             "6E50F0079C2F870EB1A8E1A477F8FF2F" };

      vt_256 = new String[] { "570C7199920DE014C1825423CEB62E03",
                             "8C26F898A7ACF62A710C3F64D50A3B63",
                             "47DEEEFEC4CC45211B1E35EE0769A3BF",
                             "4AA854C639F1585B8078B8CA7353C16B",
                             "43C80353A6461328B253769F1FCE205A" };

      mct_ecb_e_128 = new String[] { "2A6C912A488A452F558E18C5E4CFC5BA",
                                    "47B9B72583D3E71A9B3553A1E8B58257",
                                    "860D8175D8A868D34557B77A0216DE0F",
                                    "D95165C080C86E3AD303B214015E0333",
                                    "1C4D5F272E10848513C8755BF73D78EA" };

      mct_ecb_e_192 = new String[] { "BDB43001BDBD82824768D56B92403380",
                                    "6C1F7A7E54C242A9C453A7223EC7AD3C",
                                    "E4A0C4C55605B20C3FD708C526CA5D8A",
                                    "D8139FDBF39DA3F9CDB87501D9FCA820",
                                    "85935A8D05CB415F4E87B07B57ADDEF8" };

      mct_ecb_e_256 = new String[] { "0150894308014AE25FB2EA3CEEC85210",
                                    "11F33B4127A57F2FF84C10AEF5159B94",
                                    "C294BD0E643D7AEED6CBAE0CCCB6BEF0",
                                    "FF4095A5432EB9CFD4C063176ACCB373",
                                    "84C5A38B1380D76BF8D50A346B47F563" };

      mct_ecb_d_128 = new String[] { "16600BC38CD79256652598F58228EFCF",
                                    "EBA185F180C7B4DB422DA0C88CD74B26",
                                    "C73B84FD1F5ADE0FCD444FCD46F47203",
                                    "F55FF70281CB55E99CE9DE51BE142C57",
                                    "42E97BDBB4ECF18F18166BC32109BF45" };

      mct_ecb_d_192 = new String[] { "879E3B2EEADFB4EA7B6BD46C4BE5E769",
                                    "11E607AB4460F1D50BDF748E3BA9BD57",
                                    "F15ACFD9E5267D85D6AC29F33FA1CBCB",
                                    "79CC26A4393A55B28738CA3A8A299FE9",
                                    "9B769F72D8632B6ADD1427FCF4FDF2DE" };

      mct_ecb_d_256 = new String[] { "0F504BDFAB77735CA40FCEBCAF47EE92",
                                    "9A448413BC6D98ABC3CAB39B8A9E6871",
                                    "636234AE5C2DDDFA7AE1627244CFCC90",
                                    "D3456F8048F7E5482671C7E2DEA9D762",
                                    "93562D9EC63F5B797588BD91E34E76A0" };

      mct_cbc_e_128 = new String[] { "3724E80A6C3CB054FF0E518999BBAB88",
                                    "F238302711B764856DA5A092C8289F5E",
                                    "2B632C0177CCCB6B4DD958924F6730DE",
                                    "04252FB295BE5FA6D32E501BABAF67DA",
                                    "E2A899340FE1E0302687D43F6D425798" };

      mct_cbc_e_192 = new String[] { "25086B9E6DE422DF5B3A62C4B9C64568",
                                    "042A05E5BCE817A9FC45AE7FE3A7D46C",
                                    "18F2104559B375818F3010A1C2C80C36",
                                    "71AC9391D488EE51A3C8F640A00A2B5D",
                                    "780A4737C44FC3F073155DB9C97BB21E" };

      mct_cbc_e_256 = new String[] { "022A01C9EFB90CB3D77D62F176004D85",
                                    "965CE7ABB639854D2A7929F0FD1206DF",
                                    "59FE9BB4E2272D0F68C7A510738AC7FC",
                                    "1F372860A508267A67356531B9596BBA",
                                    "E186DA9BD50AA7E30000DEF5C8A448D4" };

      mct_cbc_d_128 = new String[] { "5B237C6AE5BA37765A4E3938E7569F55",
                                    "CC7CD5A28DCE22C732F2371D937A726A",
                                    "84E078514190370771366EB9CF2D7031",
                                    "A6DEC5917B36562FDE3DBCCB76FA35BD",
                                    "1BD89437F976ADF10D7BFD9D852427CC" };

      mct_cbc_d_192 = new String[] { "430231B438EB551A8AF015ED00690E14",
                                    "62C86D437D35421720FAD2771D1065EE",
                                    "DFD175DD25EAC3265BA39B42450B3406",
                                    "A7BCD306586A0DC9E7949D828776DC3A",
                                    "C56E79EC9080F036A4454BE120540778" };

      mct_cbc_d_256 = new String[] { "39D0043E180C28B46D041AD3E050A1D4",
                                    "BA6EE5D10D4626ACDC642E580892B4DA",
                                    "01B932102B6BFCE16B7D74F4137A6DDF",
                                    "6049E4832730CA8AB3CA946DC1CFC08A",
                                    "0464A26509435822A9879EF7521F45AC" };
    }

  public void test(TestHarness harness)
  {
    harness.checkPoint("TestOfAnubis");
    cipher = new Anubis();
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
        harness.fail("TestOfAnubis");
      }
  }
}