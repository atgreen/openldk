/* TestOfSha256.java -- 
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
import gnu.java.security.hash.Sha256;
import gnu.java.security.util.Util;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Conformance tests for the SHA-2-1 implementation.
 */
public class TestOfSha256 implements Testlet
{
  private IMessageDigest algorithm;

  public void test(TestHarness harness)
  {
    harness.checkPoint("TestOfSha256");
    try
      {
        algorithm = new Sha256();
        harness.check(algorithm.selfTest(), "selfTest");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfSha256.selfTest");
      }

    try
      {
        algorithm = new Sha256();
        algorithm.update(
            "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq".getBytes(),
            0, 56);
        byte[] md = algorithm.digest();
        String exp =
            "248D6A61D20638B8E5C026930C3E6039A33CE45964FF2167F6ECEDD419DB06C1";
        harness.check(exp.equals(Util.toString(md)), "testAlphabet");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfSha256.testAlphabet");
      }
  }
}