/* TestOfEAX.java -- 
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

package gnu.testlet.gnu.javax.crypto.mode;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import gnu.java.security.Registry;
import gnu.java.security.util.Util;
import gnu.javax.crypto.cipher.CipherFactory;
import gnu.javax.crypto.mode.EAX;
import gnu.javax.crypto.mode.IAuthenticatedMode;

import java.util.Arrays;
import java.util.HashMap;

public class TestOfEAX implements Testlet
{
  // (key, iv, msg, header, ciphertest, tag)
  private static byte[][][] TESTS = new byte[][][] {
      new byte[][] {
                    Util.toBytesFromString("233952DEE4D5ED5F9B9C6D6FF80FF478"),
                    Util.toBytesFromString("62EC67F9C3A4A407FCB2A8C49031A8B3"),
                    new byte[0],
                    Util.toBytesFromString("6BFB914FD07EAE6B"),
                    new byte[0],
                    Util.toBytesFromString("E037830E8389F27B025A2D6527E79D01") },

      new byte[][] {
                    Util.toBytesFromString("91945D3F4DCBEE0BF45EF52255F095A4"),
                    Util.toBytesFromString("BECAF043B0A23D843194BA972C66DEBD"),
                    Util.toBytesFromString("F7FB"),
                    Util.toBytesFromString("FA3BFD4806EB53FA"),
                    Util.toBytesFromString("19DD"),
                    Util.toBytesFromString("5C4C9331049D0BDAB0277408F67967E5") },

      new byte[][] {
                    Util.toBytesFromString("01F74AD64077F2E704C0F60ADA3DD523"),
                    Util.toBytesFromString("70C3DB4F0D26368400A10ED05D2BFF5E"),
                    Util.toBytesFromString("1A47CB4933"),
                    Util.toBytesFromString("234A3463C1264AC6"),
                    Util.toBytesFromString("D851D5BAE0"),
                    Util.toBytesFromString("3A59F238A23E39199DC9266626C40F80") },

      new byte[][] {
                    Util.toBytesFromString("D07CF6CBB7F313BDDE66B727AFD3C5E8"),
                    Util.toBytesFromString("8408DFFF3C1A2B1292DC199E46B7D617"),
                    Util.toBytesFromString("481C9E39B1"),
                    Util.toBytesFromString("33CCE2EABFF5A79D"),
                    Util.toBytesFromString("632A9D131A"),
                    Util.toBytesFromString("D4C168A4225D8E1FF755939974A7BEDE") },

      new byte[][] {
                    Util.toBytesFromString("35B6D0580005BBC12B0587124557D2C2"),
                    Util.toBytesFromString("FDB6B06676EEDC5C61D74276E1F8E816"),
                    Util.toBytesFromString("40D0C07DA5E4"),
                    Util.toBytesFromString("AEB96EAEBE2970E9"),
                    Util.toBytesFromString("071DFE16C675"),
                    Util.toBytesFromString("CB0677E536F73AFE6A14B74EE49844DD") },

      new byte[][] {
                    Util.toBytesFromString("BD8E6E11475E60B268784C38C62FEB22"),
                    Util.toBytesFromString("6EAC5C93072D8E8513F750935E46DA1B"),
                    Util.toBytesFromString("4DE3B35C3FC039245BD1FB7D"),
                    Util.toBytesFromString("D4482D1CA78DCE0F"),
                    Util.toBytesFromString("835BB4F15D743E350E728414"),
                    Util.toBytesFromString("ABB8644FD6CCB86947C5E10590210A4F") },

      new byte[][] {
                    Util.toBytesFromString("7C77D6E813BED5AC98BAA417477A2E7D"),
                    Util.toBytesFromString("1A8C98DCD73D38393B2BF1569DEEFC19"),
                    Util.toBytesFromString("8B0A79306C9CE7ED99DAE4F87F8DD61636"),
                    Util.toBytesFromString("65D2017990D62528"),
                    Util.toBytesFromString("02083E3979DA014812F59F11D52630DA30"),
                    Util.toBytesFromString("137327D10649B0AA6E1C181DB617D7F2") },

      new byte[][] {
                    Util.toBytesFromString("5FFF20CAFAB119CA2FC73549E20F5B0D"),
                    Util.toBytesFromString("DDE59B97D722156D4D9AFF2BC7559826"),
                    Util.toBytesFromString("1BDA122BCE8A8DBAF1877D962B8592DD2D56"),
                    Util.toBytesFromString("54B9F04E6A09189A"),
                    Util.toBytesFromString("2EC47B2C4954A489AFC7BA4897EDCDAE8CC3"),
                    Util.toBytesFromString("3B60450599BD02C96382902AEF7F832A") },

      new byte[][] {
                    Util.toBytesFromString("A4A4782BCFFD3EC5E7EF6D8C34A56123"),
                    Util.toBytesFromString("B781FCF2F75FA5A8DE97A9CA48E522EC"),
                    Util.toBytesFromString("6CF36720872B8513F6EAB1A8A44438D5EF11"),
                    Util.toBytesFromString("899A175897561D7E"),
                    Util.toBytesFromString("0DE18FD0FDD91E7AF19F1D8EE8733938B1E8"),
                    Util.toBytesFromString("E7F6D2231618102FDB7FE55FF1991700") },

      new byte[][] {
                    Util.toBytesFromString("8395FCF1E95BEBD697BD010BC766AAC3"),
                    Util.toBytesFromString("22E7ADD93CFC6393C57EC0B3C17D6B44"),
                    Util.toBytesFromString("CA40D7446E545FFAED3BD12A740A659FFBBB3CEAB7"),
                    Util.toBytesFromString("126735FCC320D25A"),
                    Util.toBytesFromString("CB8920F87A6C75CFF39627B56E3ED197C552D295A7"),
                    Util.toBytesFromString("CFC46AFC253B4652B1AF3795B124AB6E") } };

  public void test(TestHarness harness)
  {
    harness.checkPoint("EAX/AES-128");
    IAuthenticatedMode mode = (IAuthenticatedMode)
    //         ModeFactory.getInstance(Registry.EAX_MODE, Registry.AES_CIPHER, 16);
    new EAX(CipherFactory.getInstance(Registry.AES_CIPHER), 16);
    HashMap attr = new HashMap();
    attr.put(IAuthenticatedMode.MODE_BLOCK_SIZE, new Integer(1));
    for (int i = 0; i < TESTS.length; i++)
      {
        attr.put(IAuthenticatedMode.KEY_MATERIAL, TESTS[i][0]);
        attr.put(IAuthenticatedMode.IV, TESTS[i][1]);
        try
          {
            mode.reset();
            mode.init(attr);
            mode.update(TESTS[i][3], 0, TESTS[i][3].length);
            byte[] ct = new byte[TESTS[i][2].length];
            for (int j = 0; j < TESTS[i][2].length; j += mode.currentBlockSize())
              {
                mode.update(TESTS[i][2], j, ct, j);
              }
            byte[] tag = mode.digest();
            harness.verbose(" KEY: " + Util.toString(TESTS[i][0]));
            harness.verbose("  IV: " + Util.toString(TESTS[i][1]));
            harness.verbose(" MSG: " + Util.toString(TESTS[i][2]));
            harness.verbose(" HDR: " + Util.toString(TESTS[i][3]));
            harness.verbose(" ECT: " + Util.toString(TESTS[i][4]));
            harness.verbose("  CT: " + Util.toString(ct));
            harness.verbose("ETAG: " + Util.toString(TESTS[i][5]));
            harness.verbose(" TAG: " + Util.toString(tag));
            harness.check(Arrays.equals(TESTS[i][4], ct));
            harness.check(Arrays.equals(TESTS[i][5], tag));
          }
        catch (Exception x)
          {
            harness.fail(x.toString());
            harness.debug(x);
          }
      }
  }
}