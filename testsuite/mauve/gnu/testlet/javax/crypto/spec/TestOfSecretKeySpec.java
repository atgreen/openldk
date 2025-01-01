/* TestOfSecretKeySpec.java -- Tests for SecretKeySpec
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


package gnu.testlet.javax.crypto.spec;

import javax.crypto.spec.SecretKeySpec;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class TestOfSecretKeySpec
    implements Testlet
{

  /**
   * Tests for SecretKeySpec.
   * <p>
   * <p>
   * Note: This is not a complete coverage test for SecretKeySpec. It currently
   * only tests the Equals() method.
   */
  public void test(TestHarness harness)
  {
    testEquals(harness);
  }

  /*
   * Test to make sure that the equals method functions properly.
   */
  private void testEquals(TestHarness harness)
  {

    String algorithm = "DES";
    String algorithm2 = "AES";

    byte[] key = new byte[32];
    for (int i = 0; i < key.length; i++)
      key[i] = (byte) i;

    byte[] key2 = new byte[32];
    for (int i = 0; i < key2.length; i++)
      key2[i] = (byte) i;

    byte[] key3 = new byte[32];
    for (int i = 0; i < key3.length; i++)
      key3[i] = (byte) (i + 3);

    SecretKeySpec secretKeySpec = new SecretKeySpec(key, algorithm);
    SecretKeySpec secretKeySpec2 = new SecretKeySpec(key2, algorithm);
    SecretKeySpec secretKeySpec3 = new SecretKeySpec(key3, algorithm);
    SecretKeySpec secretKeySpec4 = new SecretKeySpec(key, algorithm2);

    // test if two similar SecretKeySpecs are equal
    try
      {
        harness.check(secretKeySpec.equals(secretKeySpec2) == true,
                      "equals(secretKeySpec2) should return true");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("equals (secretkeyspec2) : " + String.valueOf(x));
      }

    // test if two SecretKeySpecs with different keys are not equal
    try
      {
        harness.check(secretKeySpec.equals(secretKeySpec3) == false,
                      "equals(secretKeySpec3) should return false");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("equals (secretkeyspec3) : " + String.valueOf(x));
      }

    // test if two SecretKeySpecs with different algorthms are not equal
    try
      {
        harness.check(secretKeySpec.equals(secretKeySpec4) == false,
                      "equals(secretKeySpec4) should return false");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("equals (secretkeyspec4) : " + String.valueOf(x));
      }

    // Check if passing another object other than a SecretKeySpec will
    // return false
    try
      {
        harness.check(secretKeySpec.equals("Hello World") == false,
                      "equals (\"Hello World\") should have returned false");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("equals (\"Hello World\") : " + String.valueOf(x));
      }

  }

}
