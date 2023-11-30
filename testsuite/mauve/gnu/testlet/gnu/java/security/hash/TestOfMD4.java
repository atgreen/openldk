/* TestOfMD4.java -- 
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

package gnu.testlet.gnu.java.security.hash;

import gnu.java.security.hash.IMessageDigest;
import gnu.java.security.hash.MD4;
import gnu.java.security.util.Util;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Conformance tests for the MD4 implementation.
 */
public class TestOfMD4 implements Testlet
{
  private IMessageDigest algorithm, clone;

  public void test(TestHarness harness)
  {
    harness.checkPoint("TestOfMD4");
    try
      {
        algorithm = new MD4();
        harness.check(algorithm.selfTest(), "selfTest");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfMD4.selfTest");
      }

    try
      {
        algorithm = new MD4();
        algorithm.update("a".getBytes(), 0, 1);
        byte[] md = algorithm.digest();
        String exp = "BDE52CB31DE33E46245E05FBDBD6FB24";
        harness.check(exp.equals(Util.toString(md)), "testA");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfMD4.testA");
      }

    try
      {
        algorithm = new MD4();
        algorithm.update("abc".getBytes(), 0, 3);
        byte[] md = algorithm.digest();
        String exp = "A448017AAF21D8525FC10AE87AA6729D";
        harness.check(exp.equals(Util.toString(md)), "testABC");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfMD4.testABC");
      }

    try
      {
        algorithm = new MD4();
        algorithm.update("message digest".getBytes(), 0, 14);
        byte[] md = algorithm.digest();
        String exp = "D9130A8164549FE818874806E1C7014B";
        harness.check(exp.equals(Util.toString(md)), "testMessageDigest");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfMD4.testMessageDigest");
      }

    try
      {
        algorithm = new MD4();
        algorithm.update("abcdefghijklmnopqrstuvwxyz".getBytes(), 0, 26);
        byte[] md = algorithm.digest();
        String exp = "D79E1C308AA5BBCDEEA8ED63DF412DA9";
        harness.check(exp.equals(Util.toString(md)), "testAlphabet");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfMD4.testAlphabet");
      }

    try
      {
        algorithm = new MD4();
        algorithm.update(
            "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
                .getBytes(),
            0, 62);
        byte[] md = algorithm.digest();
        String exp = "043F8582F241DB351CE627E153E7F0E4";
        harness.check(exp.equals(Util.toString(md)), "testAsciiSubset");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfMD4.testAsciiSubset");
      }

    try
      {
        algorithm = new MD4();
        algorithm.update(
            "12345678901234567890123456789012345678901234567890123456789012345678901234567890"
                .getBytes(),
            0, 80);
        byte[] md = algorithm.digest();
        String exp = "E33B4DDC9C38F2199C3E7B164FCC0536";
        harness.check(exp.equals(Util.toString(md)), "testEightyNumerics");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfMD4.testEightyNumerics");
      }

    try
      {
        algorithm = new MD4();
        algorithm.update("a".getBytes(), 0, 1);
        clone = (IMessageDigest) algorithm.clone();
        byte[] md = algorithm.digest();
        String exp = "BDE52CB31DE33E46245E05FBDBD6FB24";
        harness.check(exp.equals(Util.toString(md)), "testCloning #1");

        clone.update("bc".getBytes(), 0, 2);
        md = clone.digest();
        exp = "A448017AAF21D8525FC10AE87AA6729D";
        harness.check(exp.equals(Util.toString(md)), "testCloning #2");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfMD4.testCloning");
      }
  }
}