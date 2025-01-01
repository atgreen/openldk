/* TestOfHaval.java -- 
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

import gnu.java.security.hash.Haval;
import gnu.java.security.hash.IMessageDigest;
import gnu.java.security.util.Util;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

/**
 * Conformance tests for the HAVAL (version 1.1) hash.
 *
 * <p>Certification data for this version is as follows:</p>
 *
 * <pre>
 *      HAVAL (V.1) CERTIFICATION DATA
 *      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
 *
 * PASS=3, FPTLEN=128:
 * HAVAL("") = C68F39913F901F3DDF44C707357A7D70
 *
 * PASS=3, FPTLEN=160:
 * HAVAL("a") = 4DA08F514A7275DBC4CECE4A347385983983A830
 *
 * PASS=4, FPTLEN=192:
 * HAVAL("HAVAL") = 0C1396D7772689C46773F3DAACA4EFA982ADBFB2F1467EEA
 *
 * PASS=4, FPTLEN=224:
 * HAVAL("0123456789") = BEBD7816F09BAEECF8903B1B9BC672D9FA428E462BA699F814841529
 *
 * PASS=5, FPTLEN=256:
 * HAVAL("abcdefghijklmnopqrstuvwxyz")
 *       = C9C7D8AFA159FD9E965CB83FF5EE6F58AEDA352C0EFF005548153A61551C38EE
 * HAVAL("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
 *       = B45CB6E62F2B1320E4F8F1B0B273D45ADD47C321FD23999DCF403AC37636D963
 * </pre>
 */
public class TestOfHaval implements Testlet
{
  private IMessageDigest algorithm, clone;

  byte[] md;

  String exp;

  public void test(TestHarness harness)
  {
    harness.checkPoint("TestOfHaval");
    try
      {
        algorithm = new Haval(); // 128-bit, 3-round
        harness.check(algorithm.selfTest(), "selfTest");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfHaval.selfTest");
      }

    try
      {
        algorithm = new Haval(Haval.HAVAL_128_BIT, Haval.HAVAL_3_ROUND);
        algorithm.update("".getBytes(), 0, 0);
        md = algorithm.digest();
        exp = "C68F39913F901F3DDF44C707357A7D70";
        harness.check(exp.equals(Util.toString(md)), "testEmptyString");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfHaval.testEmptyString");
      }

    try
      {
        algorithm = new Haval(Haval.HAVAL_160_BIT, Haval.HAVAL_3_ROUND);
        algorithm.update("a".getBytes(), 0, 1);
        md = algorithm.digest();
        exp = "4DA08F514A7275DBC4CECE4A347385983983A830";
        harness.check(exp.equals(Util.toString(md)), "testA");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfHaval.testA");
      }

    try
      {
        algorithm = new Haval(Haval.HAVAL_192_BIT, Haval.HAVAL_4_ROUND);
        algorithm.update("HAVAL".getBytes(), 0, 5);
        md = algorithm.digest();
        exp = "0C1396D7772689C46773F3DAACA4EFA982ADBFB2F1467EEA";
        harness.check(exp.equals(Util.toString(md)), "testHAVAL");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfHaval.testHAVAL");
      }

    try
      {
        algorithm = new Haval(Haval.HAVAL_224_BIT, Haval.HAVAL_4_ROUND);
        algorithm.update("0123456789".getBytes(), 0, 10);
        md = algorithm.digest();
        exp = "BEBD7816F09BAEECF8903B1B9BC672D9FA428E462BA699F814841529";
        harness.check(exp.equals(Util.toString(md)), "testDecimalDigits");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfHaval.testDecimalDigits");
      }

    try
      {
        algorithm = new Haval(Haval.HAVAL_256_BIT, Haval.HAVAL_5_ROUND);
        algorithm.update("abcdefghijklmnopqrstuvwxyz".getBytes(), 0, 26);
        md = algorithm.digest();
        exp = "C9C7D8AFA159FD9E965CB83FF5EE6F58AEDA352C0EFF005548153A61551C38EE";
        harness.check(exp.equals(Util.toString(md)), "testAlphabet");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfHaval.testAlphabet");
      }

    try
      {
        algorithm = new Haval(Haval.HAVAL_256_BIT, Haval.HAVAL_5_ROUND);
        algorithm.update(
            "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789".getBytes(), 0, 62);
        md = algorithm.digest();
        exp = "B45CB6E62F2B1320E4F8F1B0B273D45ADD47C321FD23999DCF403AC37636D963";
        harness.check(exp.equals(Util.toString(md)), "testAsciiSubset");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfHaval.testAsciiSubset");
      }

    try
      {
        algorithm = new Haval(Haval.HAVAL_192_BIT, Haval.HAVAL_4_ROUND);
        algorithm.update("HA".getBytes(), 0, 2);
        clone = (IMessageDigest) algorithm.clone();
        clone.update("VAL".getBytes(), 0, 3);
        md = clone.digest();
        exp = "0C1396D7772689C46773F3DAACA4EFA982ADBFB2F1467EEA";
        harness.check(exp.equals(Util.toString(md)), "testCloning");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("TestOfHaval.testCloning");
      }
  }
}