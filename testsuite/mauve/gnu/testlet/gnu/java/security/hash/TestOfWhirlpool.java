/* TestOfWhirlpool.java -- 
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
import gnu.java.security.hash.Whirlpool;
import gnu.java.security.util.Util;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Conformance tests for the {@link Whirlpool} hash implementation.
 * <p>
 * The test vectors used are those described in the "iso-test-vectors.txt"
 * document included in the Whirlpool Version 3 documentation package available
 * from
 * <a href="http://planeta.terra.com.br/informatica/paulobarreto/WhirlpoolPage.html">
 * The WHIRLPOOL Hashing Function</a> Paulo's page.
 */
public class TestOfWhirlpool implements Testlet
{
  /** The test vector for the ASCII character 'a'. */
  private static final String TV1 =
      "8ACA2602792AEC6F11A67206531FB7D7F0DFF59413145E6973C45001D0087B42"
    + "D11BC645413AEFF63A42391A39145A591A92200D560195E53B478584FDAE231A";

  /** The test vector for the ASCII characters 'abc'. */
  private static final String TV2 =
      "4E2448A4C6F486BB16B6562C73B4020BF3043E3A731BCE721AE1B303D97E6D4C"
    + "7181EEBDB6C57E277D0E34957114CBD6C797FC9D95D8B582D225292076D4EEF5";

  private IMessageDigest algorithm, clone;

  // default 0-arguments constructor

  public void test(TestHarness harness)
  {
    harness.checkPoint("TestOfWhirlpool");
    try
      {
        algorithm = new Whirlpool();
        harness.check(algorithm.selfTest(), "selfTest");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfWhirlpool.selfTest");
      }

    try
      {
        algorithm = new Whirlpool();
        algorithm.update("a".getBytes(), 0, 1);
        byte[] md = algorithm.digest();
        harness.check(TV1.equals(Util.toString(md)), "testA");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfWhirlpool.testA");
      }

    try
      {
        algorithm = new Whirlpool();
        algorithm.update("abc".getBytes(), 0, 3);
        byte[] md = algorithm.digest();
        harness.check(TV2.equals(Util.toString(md)), "testABC");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfWhirlpool.testABC");
      }

    try
      {
        algorithm = new Whirlpool();
        algorithm.update("message digest".getBytes(), 0, 14);
        byte[] md = algorithm.digest();
        String exp = "378C84A4126E2DC6E56DCC7458377AAC838D00032230F53CE1F5700C0FFB4D3B"
                   + "8421557659EF55C106B4B52AC5A4AAA692ED920052838F3362E86DBD37A8903E";
        harness.check(exp.equals(Util.toString(md)), "testMessageDigest");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfWhirlpool.testMessageDigest");
      }

    try
      {
        algorithm = new Whirlpool();
        algorithm.update("abcdefghijklmnopqrstuvwxyz".getBytes(), 0, 26);
        byte[] md = algorithm.digest();
        String exp = "F1D754662636FFE92C82EBB9212A484A8D38631EAD4238F5442EE13B8054E41B"
                   + "08BF2A9251C30B6A0B8AAE86177AB4A6F68F673E7207865D5D9819A3DBA4EB3B";
        harness.check(exp.equals(Util.toString(md)), "testAlphabet");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfWhirlpool.testAlphabet");
      }

    try
      {
        algorithm = new Whirlpool();
        algorithm.update("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789".getBytes(),
                         0,
                         62);
        byte[] md = algorithm.digest();
        String exp = "DC37E008CF9EE69BF11F00ED9ABA26901DD7C28CDEC066CC6AF42E40F82F3A1E"
                   + "08EBA26629129D8FB7CB57211B9281A65517CC879D7B962142C65F5A7AF01467";
        harness.check(exp.equals(Util.toString(md)), "testAsciiSubset");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfWhirlpool.testAsciiSubset");
      }

    try
      {
        algorithm = new Whirlpool();
        for (int i = 0; i < 8; i++)
          algorithm.update("1234567890".getBytes(), 0, 10);
        byte[] md = algorithm.digest();
        String exp = "466EF18BABB0154D25B9D38A6414F5C08784372BCCB204D6549C4AFADB601429"
                   + "4D5BD8DF2A6C44E538CD047B2681A51A2C60481E88C5A20B2C2A80CF3A9A083B";
        harness.check(exp.equals(Util.toString(md)), "testEightyNumerics");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfWhirlpool.testEightyNumerics");
      }

    try
      {
        algorithm = new Whirlpool();
        algorithm.update((byte) 'a');
        clone = (IMessageDigest) algorithm.clone();
        byte[] md = algorithm.digest();
        harness.check(TV1.equals(Util.toString(md)), "testCloning #1");

        clone.update((byte) 'b');
        clone.update((byte) 'c');
        md = clone.digest();
        harness.check(TV2.equals(Util.toString(md)), "testCloning #2");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfWhirlpool.testCloning");
      }
  }
}