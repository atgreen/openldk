/* AllCertificatesSamePoliciesTest10_3.java
   Copyright (C) 2004  Free Software Foundation, Inc.

   Distributed under the GPL; see the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseValidTest
// Files: data/certs/AllCertificatesSamePoliciesTest10EE.crt data/certs/PoliciesP12CACert.crt data/crls/PoliciesP12CACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

import java.security.cert.PKIXParameters;
import java.util.Collections;

public class AllCertificatesSamePoliciesTest10_3 extends BaseValidTest
{
  public AllCertificatesSamePoliciesTest10_3()
  {
    super (new String[] { "data/certs/AllCertificatesSamePoliciesTest10EE.crt",
                          "data/certs/PoliciesP12CACert.crt" },
           new String[] { "data/crls/PoliciesP12CACRL.crl" });
  }

  protected void setupAdditionalParams (PKIXParameters params)
  {
    params.setInitialPolicies (Collections.singleton (NIST_TEST_POLICY_2));
  }
}
