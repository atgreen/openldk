/* OverlappingPoliciesTest6_1.java
   Copyright (C) 2004  Free Software Foundation, Inc.

   Distributed under the GPL; see the file `COPYING' */

// Tags: JDK1.4

package gnu.testlet.java.security.cert.pkix.pkits;

import java.security.cert.PKIXParameters;

public class OverlappingPoliciesTest6_1 extends BaseValidTest
{
  public OverlappingPoliciesTest6_1()
  {
    super (new String[] { "data/certs/OverlappingPoliciesTest6EE.crt",
                          "data/certs/PoliciesP1234subsubCAP123P12Cert.crt",
                          "data/certs/PoliciesP1234subCAP123Cert.crt",
                          "data/certs/PoliciesP1234CACert.crt" },
           new String[] { "data/crls/PoliciesP1234subsubCAP123P12CRL.crl",
                          "data/crls/PoliciesP1234subCAP123CRL.crl",
                          "data/crls/PoliciesP1234CACRL.crl" });
  }

  protected void setupAdditionalParams (PKIXParameters params)
  {
  }
}
