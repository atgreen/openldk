/* OverlappingPoliciesTest6_2.java
   Copyright (C) 2004  Free Software Foundation, Inc.

   Distributed under the GPL; see the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseInvalidTest
// Files: data/certs/OverlappingPoliciesTest6EE.crt data/certs/PoliciesP1234subsubCAP123P12Cert.crt data/certs/PoliciesP1234subCAP123Cert.crt data/certs/PoliciesP1234CACert.crt data/crls/PoliciesP1234subsubCAP123P12CRL.crl data/crls/PoliciesP1234subCAP123CRL.crl data/crls/PoliciesP1234CACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

import java.security.cert.PKIXParameters;
import java.util.Collections;

public class OverlappingPoliciesTest6_3 extends BaseInvalidTest
{
  public OverlappingPoliciesTest6_3()
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
    params.setExplicitPolicyRequired (true);
    params.setInitialPolicies (Collections.singleton (NIST_TEST_POLICY_2));
  }
}
