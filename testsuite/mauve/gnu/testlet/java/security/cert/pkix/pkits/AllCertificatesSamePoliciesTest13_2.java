/* AllCertificatesSamePoliciesTest13_2.java
   Copyright (C) 2004  Free Software Foundation, Inc.

   Distributed under the GPL; see the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseValidTest
// Files: data/certs/AllCertificatesSamePoliciesTest13EE.crt data/certs/PoliciesP123CACert.crt data/crls/PoliciesP123CACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

import java.security.cert.PKIXParameters;
import java.util.Collections;

public class AllCertificatesSamePoliciesTest13_2 extends BaseValidTest
{
  public AllCertificatesSamePoliciesTest13_2()
  {
    super (new String[] { "data/certs/AllCertificatesSamePoliciesTest13EE.crt",
                          "data/certs/PoliciesP123CACert.crt" },
           new String[] { "data/crls/PoliciesP123CACRL.crl" });
  }

  protected void setupAdditionalParams (PKIXParameters params)
  {
    params.setInitialPolicies (Collections.singleton (NIST_TEST_POLICY_2));
  }
}
