/* TestOfOMAC.java -- 
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

package gnu.testlet.gnu.javax.crypto.mac;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import gnu.java.security.Registry;
import gnu.java.security.util.Util;
import gnu.javax.crypto.mac.IMac;
import gnu.javax.crypto.mac.MacFactory;

import java.util.Arrays;
import java.util.HashMap;

public class TestOfOMAC implements Testlet
{
  // (key, message, tag)
  public static final byte[][][] TESTS1 = new byte[][][] {
      new byte[][] {
                    Util.toBytesFromString("2b7e151628aed2a6abf7158809cf4f3c"),
                    new byte[0],
                    Util.toBytesFromString("bb1d6929e95937287fa37d129b756746") },
      new byte[][] {
                    Util.toBytesFromString("2b7e151628aed2a6abf7158809cf4f3c"),
                    Util.toBytesFromString("6bc1bee22e409f96e93d7e117393172a"),
                    Util.toBytesFromString("070a16b46b4d4144f79bdd9dd04a287c") },
      new byte[][] {
                    Util.toBytesFromString("2b7e151628aed2a6abf7158809cf4f3c"),
                    Util.toBytesFromString("6bc1bee22e409f96e93d7e117393172a"
                                           + "ae2d8a571e03ac9c9eb76fac45af8e51"
                                           + "30c81c46a35ce411"),
                    Util.toBytesFromString("dfa66747de9ae63030ca32611497c827") },
      new byte[][] {
                    Util.toBytesFromString("2b7e151628aed2a6abf7158809cf4f3c"),
                    Util.toBytesFromString("6bc1bee22e409f96e93d7e117393172a"
                                           + "ae2d8a571e03ac9c9eb76fac45af8e51"
                                           + "30c81c46a35ce411e5fbc1191a0a52ef"
                                           + "f69f2445df4f9b17ad2b417be66c3710"),
                    Util.toBytesFromString("51f0bebf7e3b9d92fc49741779363cfe") } };

  public static final byte[][][] TESTS2 = new byte[][][] {
      new byte[][] {
                    Util.toBytesFromString("8e73b0f7da0e6452c810f32b809079e5"
                                           + "62f8ead2522c6b7b"),
                    new byte[0],
                    Util.toBytesFromString("d17ddf46adaacde531cac483de7a9367") },
      new byte[][] {
                    Util.toBytesFromString("8e73b0f7da0e6452c810f32b809079e5"
                                           + "62f8ead2522c6b7b"),
                    Util.toBytesFromString("6bc1bee22e409f96e93d7e117393172a"),
                    Util.toBytesFromString("9e99a7bf31e710900662f65e617c5184") },
      new byte[][] {
                    Util.toBytesFromString("8e73b0f7da0e6452c810f32b809079e5"
                                           + "62f8ead2522c6b7b"),
                    Util.toBytesFromString("6bc1bee22e409f96e93d7e117393172a"
                                           + "ae2d8a571e03ac9c9eb76fac45af8e51"
                                           + "30c81c46a35ce411"),
                    Util.toBytesFromString("8a1de5be2eb31aad089a82e6ee908b0e") },
      new byte[][] {
                    Util.toBytesFromString("8e73b0f7da0e6452c810f32b809079e5"
                                           + "62f8ead2522c6b7b"),
                    Util.toBytesFromString("6bc1bee22e409f96e93d7e117393172a"
                                           + "ae2d8a571e03ac9c9eb76fac45af8e51"
                                           + "30c81c46a35ce411e5fbc1191a0a52ef"
                                           + "f69f2445df4f9b17ad2b417be66c3710"),
                    Util.toBytesFromString("a1d5df0eed790f794d77589659f39a11") } };

  public static final byte[][][] TESTS3 = new byte[][][] {
      new byte[][] {
                    Util.toBytesFromString("603deb1015ca71be2b73aef0857d7781"
                                           + "1f352c073b6108d72d9810a30914dff4"),
                    new byte[0],
                    Util.toBytesFromString("028962f61b7bf89efc6b551f4667d983") },
      new byte[][] {
                    Util.toBytesFromString("603deb1015ca71be2b73aef0857d7781"
                                           + "1f352c073b6108d72d9810a30914dff4"),
                    Util.toBytesFromString("6bc1bee22e409f96e93d7e117393172a"),
                    Util.toBytesFromString("28a7023f452e8f82bd4bf28d8c37c35c") },
      new byte[][] {
                    Util.toBytesFromString("603deb1015ca71be2b73aef0857d7781"
                                           + "1f352c073b6108d72d9810a30914dff4"),
                    Util.toBytesFromString("6bc1bee22e409f96e93d7e117393172a"
                                           + "ae2d8a571e03ac9c9eb76fac45af8e51"
                                           + "30c81c46a35ce411"),
                    Util.toBytesFromString("aaf3d8f1de5640c232f5b169b9c911e6") },
      new byte[][] {
                    Util.toBytesFromString("603deb1015ca71be2b73aef0857d7781"
                                           + "1f352c073b6108d72d9810a30914dff4"),
                    Util.toBytesFromString("6bc1bee22e409f96e93d7e117393172a"
                                           + "ae2d8a571e03ac9c9eb76fac45af8e51"
                                           + "30c81c46a35ce411e5fbc1191a0a52ef"
                                           + "f69f2445df4f9b17ad2b417be66c3710"),
                    Util.toBytesFromString("e1992190549f6ed5696a2c056c315410") } };

  public void test(TestHarness harness)
  {
    IMac mac = MacFactory.getInstance(Registry.OMAC_PREFIX
                                      + Registry.AES_CIPHER);
    harness.checkPoint("OMAC/AES-128");
    HashMap attr = new HashMap();
    for (int i = 0; i < TESTS1.length; i++)
      {
        attr.put(IMac.MAC_KEY_MATERIAL, TESTS1[i][0]);
        try
          {
            mac.init(attr);
            mac.update(TESTS1[i][1], 0, TESTS1[i][1].length);
            byte[] tag = mac.digest();
            harness.check(Arrays.equals(TESTS1[i][2], tag));
          }
        catch (Exception x)
          {
            harness.debug(x);
            harness.fail(x.toString());
          }
      }

    harness.checkPoint("OMAC/AES-192");
    for (int i = 0; i < TESTS2.length; i++)
      {
        attr.put(IMac.MAC_KEY_MATERIAL, TESTS2[i][0]);
        try
          {
            mac.init(attr);
            mac.update(TESTS2[i][1], 0, TESTS2[i][1].length);
            byte[] tag = mac.digest();
            harness.check(Arrays.equals(TESTS2[i][2], tag));
          }
        catch (Exception x)
          {
            harness.debug(x);
            harness.fail(x.toString());
          }
      }

    harness.checkPoint("OMAC/AES-256");
    for (int i = 0; i < TESTS3.length; i++)
      {
        attr.put(IMac.MAC_KEY_MATERIAL, TESTS3[i][0]);
        try
          {
            mac.init(attr);
            mac.update(TESTS3[i][1], 0, TESTS3[i][1].length);
            byte[] tag = mac.digest();
            harness.check(Arrays.equals(TESTS3[i][2], tag));
          }
        catch (Exception x)
          {
            harness.debug(x);
            harness.fail(x.toString());
          }
      }
  }
}