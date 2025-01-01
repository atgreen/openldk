/* AllCertificatesAnyPolicyTest11_2.java
   Copyright (C) 2004  Free Software Foundation, Inc.

   Distributed under the GPL; see the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseInvalidTest
// Files: data/certs/AllCertificatesanyPolicyTest11EE.crt data/certs/anyPolicyCACert.crt data/crls/anyPolicyCACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

import java.security.cert.PKIXParameters;
import java.util.Collections;

public class AllCertificatesAnyPolicyTest11_2 extends BaseValidTest
{
  public AllCertificatesAnyPolicyTest11_2()
  {
    super (new String[] { "data/certs/AllCertificatesanyPolicyTest11EE.crt",
                          "data/certs/anyPolicyCACert.crt" },
           new String[] { "data/crls/anyPolicyCACRL.crl" });
  }

  protected void setupAdditionalParams (PKIXParameters params)
  {
    params.setInitialPolicies (Collections.singleton (NIST_TEST_POLICY_1));
  }
}
