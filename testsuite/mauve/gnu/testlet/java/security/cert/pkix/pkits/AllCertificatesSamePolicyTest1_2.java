/* AllCertificatesSamePolicyTest1_2.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL; see the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseValidTest
// Files: data/certs/ValidCertificatePathTest1EE.crt data/certs/GoodCACert.crt data/crls/GoodCACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

import java.security.cert.PKIXParameters;
import java.util.Collections;

public class AllCertificatesSamePolicyTest1_2 extends BaseValidTest
{
  public AllCertificatesSamePolicyTest1_2()
  {
    super (new String[] { "data/certs/ValidCertificatePathTest1EE.crt",
                          "data/certs/GoodCACert.crt" },
           new String[] { "data/crls/GoodCACRL.crl" });
  }

  protected void setupAdditionalParams (PKIXParameters params)
  {
    params.setExplicitPolicyRequired (true);
    params.setInitialPolicies (Collections.singleton (NIST_TEST_POLICY_1));
  }
}
