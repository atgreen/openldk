/* TestOfHMacMD5.java -- 
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

import gnu.java.security.util.Util;
import gnu.javax.crypto.mac.IMac;
import gnu.javax.crypto.mac.MacFactory;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.security.InvalidKeyException;
import java.util.Arrays;
import java.util.HashMap;

/**
 * Conformance tests of the HMAC-MD5 message authentication code algorithms.
 *
 * <p>References:</p>
 * <ol>
 *    <li>P. Cheng and R. Glenn, <a href="http://www.ietf.org/rfc/rfc2202.txt">RFC
 *    2202: Test Cases for HMAC-MD5 and HMAC-SHA-1</a>.</li>
 * </ol>
 */
public class TestOfHMacMD5 implements Testlet
{
  private static final byte[][][] TEST_VECTOR = {
      {
        "Jefe".getBytes(),
        "what do ya want for nothing?".getBytes(),
        Util.toBytesFromString("750c783e6ab0b503eaa86e310a5db738") },
      {
        Util.toBytesFromString("0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b"),
        "Hi There".getBytes(),
        Util.toBytesFromString("9294727a3638bb1c13f48ef8158bfc9d") },
      {
        Util.toBytesFromString("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"),
        new byte[50] /* filled in below, 0xDD 50 times */,
        Util.toBytesFromString("56be34521d144c88dbb8c733f0e8b3f6") },
      {
        Util.toBytesFromString("0102030405060708090a0b0c0d0e0f10111213141516171819"),
        new byte[50] /* filled in below, 0xCD 50 times */,
        Util.toBytesFromString("697eaf0aca3a3aea3a75164746ffaa79") },
      {
        Util.toBytesFromString("0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c"),
        "Test With Truncation".getBytes(),
        Util.toBytesFromString("56461ef2342edc00f9bab995690efd4c") },
      {
        new byte[80] /* filled in below, 0xAA 80 times */,
        "Test Using Larger Than Block-Size Key - Hash Key First".getBytes(),
        Util.toBytesFromString("6b1ab7fe4bd7bf8f0b62e6ce61b9d0cd") },
      {
        new byte[80] /* filled in below, 0xAA 80 times */,
        "Test Using Larger Than Block-Size Key and Larger Than One Block-Size Data".getBytes(),
        Util.toBytesFromString("6f630fad67cda0ee1fb1f562db3aa53e") } };

  static
    {
      int i = 0;
      for (; i < 50; i++)
        {
          TEST_VECTOR[2][1][i] = (byte) 0xDD;
          TEST_VECTOR[3][1][i] = (byte) 0xCD;

          TEST_VECTOR[5][0][i] = (byte) 0xAA;
          TEST_VECTOR[6][0][i] = (byte) 0xAA;
        }
      for (; i < 80; i++)
        {
          TEST_VECTOR[5][0][i] = (byte) 0xAA;
          TEST_VECTOR[6][0][i] = (byte) 0xAA;
        }
    }

  private HashMap attr = new HashMap();

  private IMac mac;

  public void test(TestHarness harness)
  {
    harness.checkPoint("TestOfHMacMD5");
    mac = MacFactory.getInstance("hmac-md5");
    // 1st vector SHOULD fail with key too short exception
    try
      {
        attr.put(IMac.MAC_KEY_MATERIAL, TEST_VECTOR[0][0]);
        mac.init(attr);
        mac.update(TEST_VECTOR[0][1], 0, TEST_VECTOR[0][1].length);
        harness.check(Arrays.equals(mac.digest(), TEST_VECTOR[0][2]));
        harness.fail("#0 - SHOULD have caused a Key too short exception but didn't");
      }
    catch (InvalidKeyException x)
      {
        harness.check(true, "#0");
      }

    for (int i = 1; i < TEST_VECTOR.length; i++)
      {
        try
          {
            attr.put(IMac.MAC_KEY_MATERIAL, TEST_VECTOR[i][0]);
            mac.init(attr);
            mac.update(TEST_VECTOR[i][1], 0, TEST_VECTOR[i][1].length);
            harness.check(Arrays.equals(mac.digest(), TEST_VECTOR[i][2]), "#"
                                                                          + i);
          }
        catch (Exception x)
          {
            harness.debug(x);
            harness.fail("#" + i + " - " + String.valueOf(x));
          }
      }
  }
}