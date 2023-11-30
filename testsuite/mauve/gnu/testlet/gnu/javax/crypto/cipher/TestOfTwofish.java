/* TestOfTwofish.java -- 
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
import gnu.javax.crypto.cipher.Twofish;
import gnu.testlet.TestHarness;

import java.util.HashMap;

/**
 * Conformance tests for the {@link Twofish} implementation.
 */
public class TestOfTwofish extends BaseCipherTestCase
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
      vk_128 = new String[] { "6BFD32804A1C3206C4BF85EB11241F89",
                             "F097147AE851845984DC97D5FAE40CF9",
                             "6117F1977C5ABD9647C56544D9458444",
                             "75A6240AAE357DEDDF99936705618284",
                             "F026BFDF6BFBC7E50C46C533BD271C24" };

      vk_192 = new String[] { "B5AED133641004F4121B66E7DB8F2FF0",
                             "998110F200555A32C6C123E66CF87DE9",
                             "2DBAEEEC682DCC957C2D51B0990E123A",
                             "BAEC0A31F6557D6D13B888A94F63058C",
                             "E51ADC9773E785730586E6812A0F0FA5" };

      vk_256 = new String[] { "785229B51B515F30A1FCC88B969A4E47",
                             "B095E0619E70CDF5F4BC6E88079CF22F",
                             "44F32AEAE82516AC8857C1985B7109EC",
                             "B2BBE93B433C8F0415B90282E788C071",
                             "9E953EBAA3B13F43F90908B53DAA0C09" };

      vt_128 = new String[] { "73B9FF14CF2589901FF52A0D6F4B7EDE",
                             "F5A9150BAB6D6AEBD6B4F97D9E93B28B",
                             "C30F8B221FD6D3996F973CDCDC6E305C",
                             "D6A531FE826CB0454F2D567A20018CB7",
                             "B62324BE427332A6089C7BE40D40292E" };

      vt_192 = new String[] { "62EF193EDB7D399ACA50EC1CBE5398D8",
                             "E7A58D547688BA8B69DA949E38AA6FAD",
                             "71579F70A8EDB2BA5C00C513E2D7DEEB",
                             "C6171EF892F8224DC5FAE230AF629F52",
                             "C6A61053C48D7ECD7DDD12DB0F316AD7" };

      vt_256 = new String[] { "23A385F617F313DAC05BCB7EABD61807",
                             "35BE2B4738602A1DA3DE5C9E7E871923",
                             "03E8BB7A568E95BA792DCE77D5523C2B",
                             "D3ACBE92C482D2E806FD837E41DBB288",
                             "DC3B1C37C69B4059EAADF03FCD016EB4" };

      mct_ecb_e_128 = new String[] { "282BE7E4FA1FBDC29661286F1F310B7E",
                                    "C8E1D477621ACC37742BD16032075654",
                                    "D5187E7D6B8BE9517DAC4A8AF4A552EA",
                                    "211B6F0C6061068D203440ADEFC45BAB",
                                    "E94DB85DAE438578A462277AC251F102" };

      mct_ecb_e_192 = new String[] { "9AB71D7F280FF79F0D135BBD5FAB7E37",
                                    "A1A3C49FD659216172CDE292CE5F5226",
                                    "056E89A3D7BE7634B640B843DA265D9C",
                                    "66C7C98F2D871D060303E0841FD7E691",
                                    "98661C46398A58AF3C15B0B90F6A82CD" };

      mct_ecb_e_256 = new String[] { "04F2F36CA927AE506931DE8F78B2513C",
                                    "04EAD2A58C67FEFCD4485D231C7AE4D7",
                                    "01BF7F83A2CF4D8F31B5F913FB372389",
                                    "41C1A5A2CEB3F7095E795DBCEE0F90F2",
                                    "1E3A44C7BC6EE153874B6A462676B494" };

      mct_ecb_d_128 = new String[] { "21D3F7F6724513946B72CFAE47DA2EED",
                                    "DD7D3CBE24EC771704F531ED82CE8AEE",
                                    "A24CDF52A32A27AA1700AD46A70C44AB",
                                    "C46DA91E739DDB027E8D06CE28E77478",
                                    "AAD1229FE482AAB37371C029B42F9D2C" };

      mct_ecb_d_192 = new String[] { "B4582FA55072FCFEF538F39072F234A9",
                                    "6F168851543EA0ADAF932CD68A3C2563",
                                    "02CE0B206F0560B84AB8CEB08685056B",
                                    "9FBE781AC7240292C4B9D2137EF9BB80",
                                    "189CCF7178D4FBD43AE162941241157E" };

      mct_ecb_d_256 = new String[] { "BC7D078C4872063869DEAB891FB42761",
                                    "AB2F67D3AA11747C96CA942CE40925E1",
                                    "2CEF3C1F8FE43F34456007F87DD4D710",
                                    "6F1294DE3371D9E33652A7D7C2CC8C8F",
                                    "B8C275452476F61F0EEF0651E4949795" };

      mct_cbc_e_128 = new String[] { "3CC3B181E1495D0495D652B66921DA0F",
                                    "695250B109C6F71D410AC38B0BBDA3D2",
                                    "0338A3EDB1DF10B464D3AF2C01A803BB",
                                    "FE5C77C4C15AB0894FEC0A1FE993B90C",
                                    "22BAF21C93BD8A04FB57E6753D7A0EB0" };

      mct_cbc_e_192 = new String[] { "A9F5F1AE592B31D73070C930C766FAC4",
                                    "B6F6D6B0A73B6A24179795BFBEF14B7B",
                                    "EA1BBAC95B06F3E7A08798C03E7DBA59",
                                    "154F9F2954E62A88C8DCA50A7EEE6F2E",
                                    "2EC290F031CC013653604C80AAD1D378" };

      mct_cbc_e_256 = new String[] { "EA7162E65490B03B1AE4871FB35EF23B",
                                    "549FF6C6274F034211C31FADF3F22571",
                                    "CF222616B0E4F8E48967D769456B916B",
                                    "957108025BFD57125B40057BC2DE4FE2",
                                    "6F725C5950133F82EF021A94CADC8508" };

      mct_cbc_d_128 = new String[] { "329B242F4ED7DF3B025472F409508C6E",
                                    "39408154CE557E72A4BBE3FCA83372A2",
                                    "8AB110F9DB27FA8ADD77C3BAFA171A91",
                                    "F54B5DB20DEBB29D09443C284D92E58A",
                                    "6E0D8F89398D2525316E07409ACD12C0" };

      mct_cbc_d_192 = new String[] { "26107A7854B5D887B3AAC17909D7D5C9",
                                    "C2628E2DB97B62247927A4C9FFEB5BDA",
                                    "06B781BC75DD89B63BAB68231097F043",
                                    "7699C76E2D7048988D9A89C3C79622EC",
                                    "D6F20664C25F122269E798DAC84F7DDF" };

      mct_cbc_d_256 = new String[] { "2CBA6271A1044F90C30BA8FE91E1C163",
                                    "05F05148EF495836AB0DA226B2E9D0C2",
                                    "A792AC61E7110C434BC2BBCAB6E53CAE",
                                    "4C81F5BDC1081170FF96F50B1F76A566",
                                    "BD959F5B787037631A37051EA5F369F8" };
    }

  public void test(TestHarness harness)
  {
    harness.checkPoint("TestOfTwofish");
    cipher = new Twofish();
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
        harness.fail("TestOfTwofish");
      }
  }
}