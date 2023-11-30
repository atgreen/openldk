/* TestOfCascade.java -- 
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

package gnu.testlet.gnu.javax.crypto.assembly;

import gnu.java.security.Registry;
import gnu.java.security.util.Util;
import gnu.javax.crypto.assembly.Cascade;
import gnu.javax.crypto.assembly.Direction;
import gnu.javax.crypto.assembly.Stage;
import gnu.javax.crypto.cipher.IBlockCipher;
import gnu.javax.crypto.cipher.DES;
import gnu.javax.crypto.cipher.TripleDES;
import gnu.javax.crypto.mode.ModeFactory;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.security.InvalidKeyException;
import java.util.Arrays;
import java.util.HashMap;

/**
 * Simple test of {@link Cascade} that simulates a DES-EDE constructed from
 * three separate DES instances.
 */
public class TestOfCascade implements Testlet
{
  /** The ECB encryption monte-carlo tests. */
  static final String[][] E_TV = {
      // key bytes
      // plain bytes         cipher bytes
      { "0123456789abcdef", "0123456789abcdef", "0123456789abcdef",
        "4e6f772069732074", "6a2a19f41eca854b" },
      { "0123456789abcdef", "23456789abcdef01", "0123456789abcdef",
        "4e6f772069732074", "03e69f5bfa58eb42" },
      { "0123456789abcdef", "23456789abcdef01", "456789abcdef0123",
        "4e6f772069732074", "dd17e8b8b437d232" },
      { "6b085d92976149a4", "6b085d92976149a4", "6b085d92976149a4",
        "6a2a19f41eca854b", "ce5d6c7b63177c18" },
      { "02c4da3d73f226ad", "1cbce0f2bacd3b15", "02c4da3d73f226ad",
        "03e69f5bfa58eb42", "262a60f9743e1fd8" },
      { "dc34addf3d9d1fdc", "976d456702cef4fd", "ad49c2ba0b2f975b",
        "dd17e8b8b437d232", "3145bcfc1c19382f" } };

  /** The ECB decryption monte-carlo tests. */
  static final String[][] D_TV = {
      { "0123456789abcdef", "0123456789abcdef", "0123456789abcdef",
        "4e6f772069732074", "cdd64f2f9427c15d" },
      { "0123456789abcdef", "23456789abcdef01", "0123456789abcdef",
        "4e6f772069732074", "6996c8fa47a2abeb" },
      { "0123456789abcdef", "23456789abcdef01", "456789abcdef0123",
        "4e6f772069732074", "8325397644091a0a" },
      { "cdf40b491c8c0db3", "cdf40b491c8c0db3", "cdf40b491c8c0db3",
        "cdd64f2f9427c15d", "5bb675e3db3a7f3b" },
      { "68b58c9dce086704", "529dce3719e9e0da", "68b58c9dce086704",
        "6996c8fa47a2abeb", "6b177e016e6ae12d" },
      { "83077c10cda2d6e5", "296240fd8c834fcd", "8fdac4fbe5ae978f",
        "8325397644091a0a", "c67901abdc008c89" } };

  public void test(TestHarness harness)
  {
    harness.checkPoint("TestOfCascade");

    byte[] pt, ct;
    byte[] ct1 = new byte[8];
    byte[] ct2 = new byte[8];

    HashMap map = new HashMap();
    IBlockCipher desEDE = new TripleDES();

    HashMap map1 = new HashMap();
    HashMap map2 = new HashMap();
    HashMap map3 = new HashMap();
    Cascade new3DES = new Cascade();
    Object des1 = new3DES.append(Stage.getInstance(
        ModeFactory.getInstance(
            Registry.ECB_MODE, new DES(), 8),
            Direction.FORWARD));
    Object des2 = new3DES.append(Stage.getInstance(
        ModeFactory.getInstance(
            Registry.ECB_MODE, new DES(), 8),
            Direction.REVERSED));
    Object des3 = new3DES.append(Stage.getInstance(
        ModeFactory.getInstance(
            Registry.ECB_MODE, new DES(), 8),
            Direction.FORWARD));

    map.put(des1, map1);
    map.put(des2, map2);
    map.put(des3, map3);

    for (int i = 0; i < E_TV.length; i++)
      {
        map1.put(IBlockCipher.KEY_MATERIAL, Util.toBytesFromString(E_TV[i][0]));
        map2.put(IBlockCipher.KEY_MATERIAL, Util.toBytesFromString(E_TV[i][1]));
        map3.put(IBlockCipher.KEY_MATERIAL, Util.toBytesFromString(E_TV[i][2]));
        map.put(IBlockCipher.KEY_MATERIAL,
                Util.toBytesFromString(E_TV[i][0] + E_TV[i][1] + E_TV[i][2]));
        map.put(Cascade.DIRECTION, Direction.FORWARD);
        pt = Util.toBytesFromString(E_TV[i][3]);
        ct = Util.toBytesFromString(E_TV[i][4]);

        try
          {
            desEDE.reset();
            new3DES.reset();

            desEDE.init(map);
            new3DES.init(map);

            desEDE.encryptBlock(pt, 0, ct1, 0);
            new3DES.update(pt, 0, ct2, 0);
            harness.check(Arrays.equals(ct1, ct2));

            for (int j = 0; j < 9999; j++)
              {
                desEDE.encryptBlock(ct1, 0, ct1, 0);
                new3DES.update(ct2, 0, ct2, 0);
              }
            harness.check(Arrays.equals(ct, ct1));
            harness.check(Arrays.equals(ct, ct2));
          }
        catch (InvalidKeyException x)
          {
            harness.fail("init (encryption)");
            harness.debug(x);
          }
      }

    for (int i = 0; i < D_TV.length; i++)
      {
        map1.put(IBlockCipher.KEY_MATERIAL, Util.toBytesFromString(D_TV[i][0]));
        map2.put(IBlockCipher.KEY_MATERIAL, Util.toBytesFromString(D_TV[i][1]));
        map3.put(IBlockCipher.KEY_MATERIAL, Util.toBytesFromString(D_TV[i][2]));
        map.put(IBlockCipher.KEY_MATERIAL,
                Util.toBytesFromString(D_TV[i][0] + D_TV[i][1] + D_TV[i][2]));
        map.put(Cascade.DIRECTION, Direction.REVERSED);
        pt = Util.toBytesFromString(D_TV[i][3]);
        ct = Util.toBytesFromString(D_TV[i][4]);

        try
          {
            desEDE.reset();
            new3DES.reset();

            desEDE.init(map);
            new3DES.init(map);

            desEDE.decryptBlock(pt, 0, ct1, 0);
            new3DES.update(pt, 0, ct2, 0);
            harness.check(Arrays.equals(ct1, ct2));

            for (int j = 0; j < 9999; j++)
              {
                desEDE.decryptBlock(ct1, 0, ct1, 0);
                new3DES.update(ct2, 0, ct2, 0);
              }
            harness.check(Arrays.equals(ct, ct1));
            harness.check(Arrays.equals(ct, ct2));
          }
        catch (InvalidKeyException x)
          {
            harness.fail("init (decryption)");
            harness.debug(x);
          }
      }
  }
}