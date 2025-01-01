/* TestOfPBEKeySpec.java -- Test for PBEKeySpec
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

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.Arrays;

import javax.crypto.spec.PBEKeySpec;

public class TestOfPBEKeySpec
    implements Testlet
{
  /* (non-Javadoc)
   * @see gnu.testlet.Testlet#test(gnu.testlet.TestHarness)
   */
  public void test(TestHarness harness)
  {
    testConstructorP(harness);
    testConstructorPSI(harness);
    testConstructorPSIK(harness);
    testPassword(harness);
    testSalt(harness);
    testIterationCount(harness);
    testKeyLength(harness);
  }

  /**
   * Test the constructor PBEKeySpec(char[] password).
   */
  private void testConstructorP(TestHarness harness)
  {
    char[] password = "HelloWorld".toCharArray();
    try
    {
      PBEKeySpec pbeKeySpec = new PBEKeySpec(password);

      harness.check(pbeKeySpec.getSalt() == null,
                    "salt MUST have a default value of null");
      harness.check(pbeKeySpec.getIterationCount() == 0,
                    "iterationCount MUST have a default value of 0");
      harness.check(pbeKeySpec.getKeyLength() == 0,
                    "keyLength MUST have a default value of 0");
    }
  catch (Exception x)
    {
      harness.debug(x);
      harness.fail("PBEKeySpec() with valid password failed: " + x);
    }

  try
    {
      PBEKeySpec pbeKeySpec = new PBEKeySpec(null);

      char[] pbePassword = pbeKeySpec.getPassword();
      harness.check(pbePassword.length == 0,
                    "a null password MUST produce an empty char array");
    }
  catch (Exception x)
    {
      harness.debug(x);
      harness.fail(String.valueOf(x));
    }
  }

  /**
   * Test the constructor
   * PBEKeySpec(char[] password, byte[] salt, int iterationCount).
   */
  private void testConstructorPSI(TestHarness harness)
  {
    char[] password = "HelloWorld".toCharArray();
    byte[] salt = new byte[] { 0, 1, 1, 2, 3, 5, 8 };
    int iterationCount = 102;
    try
      {
        PBEKeySpec pbeKeySpec = new PBEKeySpec(password, salt, iterationCount);

        harness.check(pbeKeySpec.getKeyLength() == 0,
                      "keyLength MUST have a default value of 0");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("PBEKeySpec() with valid password, salt and "
                     + "iterationCount failed:" + x);
      }

    try
      {
        PBEKeySpec pbeKeySpec = new PBEKeySpec(null, salt, iterationCount);
        char[] pbePassword = pbeKeySpec.getPassword();
        harness.check(pbePassword.length == 0,
                      "a null password MUST produce an empty char array");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail(String.valueOf(x));
      }

    String msg = "PBEKeySpec() MUST throw NullPointerException if salt is null";
    try
      {
        new PBEKeySpec(password, null, iterationCount);
        harness.fail(msg);
      }
    catch (NullPointerException npe)
      {
        harness.check(true, msg);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail(String.valueOf(x));
      }

    msg = "PBEKeySpec() MUST throw IllegalArgumentException if salt is an "
          + "empty array";
    try
      {
        byte[] emptySalt = new byte[0];
        new PBEKeySpec(password, emptySalt, iterationCount);
        harness.fail(msg);
      }
    catch (IllegalArgumentException iae)
      {
        harness.check(true, msg);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail(String.valueOf(x));
      }

    msg = "PBEKeySpec() MUST throw IllegalArgumentException if iterationCount "
          + "is negative";
    try
      {
        new PBEKeySpec(password, salt, - 1);
        harness.fail(msg);
      }
    catch (IllegalArgumentException iae)
      {
        harness.check(true, msg);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail(String.valueOf(x));
      }
  }

  /**
   * Test the constructor
   * PBEKeySpec(char[] password, byte[] salt, int iterationCount, int keyLength).
   */
  private void testConstructorPSIK(TestHarness harness)
  {
    char[] password = "HelloWorld".toCharArray();
    byte[] salt = new byte[] { 0, 1, 1, 2, 3, 5, 8 };
    int iterationCount = 102;
    int keyLength = 4;
    try
    {
      new PBEKeySpec(password, salt, iterationCount, keyLength);
    }
  catch (Exception x)
    {
      harness.debug(x);
      harness.fail("PBEKeySpec() with valid password, salt, iterationCount "
                   + "and keyLength failed: "  + x);
    }

  try
    {
      PBEKeySpec pbeKeySpec = new PBEKeySpec(null, salt, iterationCount,
                                             keyLength);
      char[] pbePassword = pbeKeySpec.getPassword();
      harness.check(pbePassword.length == 0,
                    "a null password MUST produce an empty char array");
    }
  catch (Exception x)
    {
      harness.debug(x);
      harness.fail("PBEKeySpec(password, salt, iterationcount, keyLength) "
                   + "with a null password failed: " + x);
    }

  String msg = "PBEKeySpec() MUST throw NullPointerException if salt is null";
  try
    {
      new PBEKeySpec(password, null, iterationCount, keyLength);
      harness.fail(msg);
    }
  catch (NullPointerException npe)
    {
      harness.check(true, msg);
    }
  catch (Exception x)
    {
      harness.debug(x);
      harness.fail(String.valueOf(x));
    }

  msg = "PBEKeySpec() MUST throw IllegalArgumentException if salt is an "
        + "empty array";
  try
    {
      byte[] emptySalt = new byte[0];
      new PBEKeySpec(password, emptySalt, iterationCount, keyLength);
      harness.fail(msg);
    }
  catch (IllegalArgumentException iae)
    {
      harness.check(true, msg);
    }
  catch (Exception x)
    {
      harness.debug(x);
      harness.fail(String.valueOf(x));
    }

  msg = "PBEKeySpec() MUST throw IllegalArgumentException if iterationCount "
        + "is negative";
  try
    {
      new PBEKeySpec(password, salt, - 1, keyLength);
      harness.fail(msg);
    }
  catch (IllegalArgumentException iae)
    {
      harness.check(true, msg);
    }
  catch (Exception x)
    {
      harness.debug(x);
      harness.fail(String.valueOf(x));
    }

  msg = "PBEKeySpec() MUST throw IllegalArgumentException if keyLength is "
        + "negative";
  try
    {
      new PBEKeySpec(password, salt, iterationCount, -1);
      harness.fail(msg);
    }
  catch (IllegalArgumentException iae)
    {
      harness.check(true, msg);
    }
  catch (Exception x)
    {
      harness.debug(x);
      harness.fail(String.valueOf(x));
    }
  }

  /**
   * Test that PBEKeySpec is indeed storing a copy of the password and not the
   * password itself.
   */
  private void testPassword(TestHarness harness)
  {
    char[] password = "HelloWorld".toCharArray();
    try
      {
        PBEKeySpec pbeKeySpec = new PBEKeySpec(password);

        // check to make sure PBEKeySpec password copy matches
        harness.check(Arrays.equals(pbeKeySpec.getPassword(), password),
                      "Value returned from getPassword() MUST equal the actual "
                      + "password");

        char[] passwordCopy = pbeKeySpec.getPassword();
        for (int i = 0; i < passwordCopy.length; i++)
          passwordCopy[i] = 'a';
        char[] originalPassword = "HelloWorld".toCharArray();

        harness.check(Arrays.equals(password, originalPassword),
                      "Changing the stored password changed the actual "
                      + "password. MUST store a COPY of the password");
        harness.check(Arrays.equals(pbeKeySpec.getPassword(), originalPassword),
                      "Changing the value returned from getPassword() changed"
                      + " the stored password. MUST return a COPY of the "
                      + "password");
        // this should clear just the copy of password, not the password itself
        pbeKeySpec.clearPassword();
        harness.check(Arrays.equals(password, "HelloWorld".toCharArray()),
                      "clearPassword() cleared the actual password. MUST store "
                      + "a COPY of the password");

        String msg = "Calling getPassword() after clearPassword() MUST throw "
                     + "IllegalStateException";
        try
          {
            pbeKeySpec.getPassword();
            harness.fail(msg);
          }
        catch (IllegalStateException ise)
          {
            harness.check(true, msg);
          }
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("testPassword(): " + x);
      }
  }

  /**
   * Test that PBEKeySpec is indeed storing a copy of the salt and not the salt
   * itself.
   */
  private void testSalt(TestHarness harness)
  {
    char[] password = "HelloWorld".toCharArray();
    byte[] salt = new byte[] { 0, 1, 1, 2, 3, 5, 8 };
    int iterationCount = 102;
    try
      {
        PBEKeySpec pbeKeySpec = new PBEKeySpec(password, salt, iterationCount);

        // check to make sure the PBEKeySpec password copy matches
        harness.check(Arrays.equals(pbeKeySpec.getSalt(), salt),
                      "Value returned from getSalt() MUST equal actual salt");

        // check that PBEKeySpec is indeed storing only a copy of the salt
        byte[] saltCopy = pbeKeySpec.getSalt();
        for (int i = 0; i < saltCopy.length; i++)
          saltCopy[i] = (byte) i;
        byte[] originalSalt = new byte[] { 0, 1, 1, 2, 3, 5, 8 };
        harness.check(Arrays.equals(salt, originalSalt),
                      "Changing the stored salt changed the actual salt. "
                      + "MUST store a COPY of the salt");
        harness.check(Arrays.equals(pbeKeySpec.getSalt(), originalSalt),
                      "Changing the value returned from getSalt() changed the "
                      + "stored salt. MUST return a COPY of the salt");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("testSalt(): " + x);
      }
  }

  /**
   * Quick tests to verify PBEKeySpec is storing the proper IterationCount.
   */
  private void testIterationCount(TestHarness harness)
  {
    char[] password = "HelloWorld".toCharArray();
    byte[] salt = new byte[] { 0, 1, 1, 2, 3, 5, 8 };
    int iterationCount = 102;
    String msg = "getIterationCount() MUST match iterationCount";
    try
      {
        PBEKeySpec pbeKeySpec = new PBEKeySpec(password, salt, iterationCount);
        harness.check(pbeKeySpec.getIterationCount() == iterationCount, msg);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("testIterationCount(): " + x);
      }
  }

  /**
   * Quick tests to verify PBEKeySpec is storing the proper KeyLength.
   */
  private void testKeyLength(TestHarness harness)
  {
    char[] password = "HelloWorld".toCharArray();
    byte[] salt = new byte[] { 0, 1, 1, 2, 3, 5, 8 };
    int iterationCount = 102;
    int keyLength = 4;
    String msg = "getKeyLength() MUST match keyLength";
    try
      {
        PBEKeySpec pbeKeySpec = new PBEKeySpec(password, salt, iterationCount,
                                               keyLength);
        harness.check(pbeKeySpec.getKeyLength() == keyLength, msg);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("testKeyLength(): " + x);
      }
  }
}
