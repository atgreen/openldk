/* TestOfPR27228.java
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

package gnu.testlet.gnu.javax.crypto.jce;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.math.BigInteger;
import java.security.KeyPairGenerator;

import javax.crypto.spec.DHParameterSpec;

/**
 * Regression test for PR Classpath/27228.
 */
public class TestOfPR27228
    implements Testlet
{
  public void test(TestHarness harness)
  {
    harness.checkPoint("TestOfPR27228()");
    try
    {
      KeyPairGenerator myKpairGen = KeyPairGenerator.getInstance("DH");
      BigInteger p = new BigInteger("17976931348623159077083915679378745319786"
                                    + "029604875601170644442368419718021615851"
                                    + "936894783379586492554150218056548598050"
                                    + "364644054819923910005079287700335581663"
                                    + "922955313623907650873575991482257486257"
                                    + "500742530207744771258955095793777842444"
                                    + "242661733472762929938766870920560605027"
                                    + "0810842907692932019128194467627007");
      BigInteger g = new BigInteger("2");
      DHParameterSpec dhSkipParamSpec = new DHParameterSpec(p, g);

      myKpairGen.initialize(dhSkipParamSpec);
      harness.check(true,
                    "MUST be able to initialize a DH key-pair generator from a "
                    + "DHParameterSpec");
      myKpairGen.generateKeyPair();
      harness.check(true, "MUST be able to generate a DH key-pair");
    }
    catch (Exception x)
    {
      harness.debug(x);
      harness.fail("TestOfPR27228(): " + x);
    }
  }
}
