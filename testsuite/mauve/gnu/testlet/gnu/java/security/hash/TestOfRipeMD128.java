/* TestOfRipeMD128.java -- 
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
import gnu.java.security.hash.RipeMD128;
import gnu.java.security.util.Util;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Conformance tests for the RipeMD128 implementation.
 */
public class TestOfRipeMD128 implements Testlet
{
  private IMessageDigest algorithm, clone;

  public void test(TestHarness harness)
  {
    harness.checkPoint("TestOfRipeMD128");
    try
      {
        algorithm = new RipeMD128();
        harness.check(algorithm.selfTest(), "selfTest");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfRipeMD128.selfTest");
      }

    try
      {
        algorithm = new RipeMD128();
        algorithm.update("a".getBytes(), 0, 1);
        byte[] md = algorithm.digest();
        String exp = "86BE7AFA339D0FC7CFC785E72F578D33";
        harness.check(exp.equals(Util.toString(md)), "testA");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfRipeMD128.testA");
      }

    try
      {
        algorithm = new RipeMD128();
        algorithm.update("abc".getBytes(), 0, 3);
        byte[] md = algorithm.digest();
        String exp = "C14A12199C66E4BA84636B0F69144C77";
        harness.check(exp.equals(Util.toString(md)), "testABC");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfRipeMD128.testABC");
      }

    try
      {
        algorithm = new RipeMD128();
        algorithm.update("message digest".getBytes(), 0, 14);
        byte[] md = algorithm.digest();
        String exp = "9E327B3D6E523062AFC1132D7DF9D1B8";
        harness.check(exp.equals(Util.toString(md)), "testMessageDigest");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfRipeMD128.testMessageDigest");
      }

    try
      {
        algorithm = new RipeMD128();
        algorithm.update("abcdefghijklmnopqrstuvwxyz".getBytes(), 0, 26);
        byte[] md = algorithm.digest();
        String exp = "FD2AA607F71DC8F510714922B371834E";
        harness.check(exp.equals(Util.toString(md)), "testAlphabet");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfRipeMD128.testAlphabet");
      }

    try
      {
        algorithm = new RipeMD128();
        algorithm.update("a".getBytes(), 0, 1);
        clone = (IMessageDigest) algorithm.clone();
        byte[] md = algorithm.digest();
        String exp = "86BE7AFA339D0FC7CFC785E72F578D33";
        harness.check(exp.equals(Util.toString(md)), "testCloning #1");

        clone.update("bc".getBytes(), 0, 2);
        md = clone.digest();
        exp = "C14A12199C66E4BA84636B0F69144C77";
        harness.check(exp.equals(Util.toString(md)), "testCloning #2");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfRipeMD128.testCloning");
      }
  }
}