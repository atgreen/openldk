/* ValidTwoCRLsTest7.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseValidTest
// Files: data/certs/ValidTwoCRLsTest7EE.crt data/certs/TwoCRLsCACert.crt data/crls/TwoCRLsCAGoodCRL.crl data/crls/TwoCRLsCABadCRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class ValidTwoCRLsTest7 extends BaseValidTest
{
  public ValidTwoCRLsTest7()
  {
    super(new String[] { "data/certs/ValidTwoCRLsTest7EE.crt",
                         "data/certs/TwoCRLsCACert.crt" },
          new String[] { "data/crls/TwoCRLsCAGoodCRL.crl",
                         "data/crls/TwoCRLsCABadCRL.crl" });
  }
}
