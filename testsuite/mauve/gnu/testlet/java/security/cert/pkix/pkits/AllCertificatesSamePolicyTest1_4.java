/* AllCertificatesSamePolicyTest1_4.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL; see the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseValidTest
// Files: data/certs/ValidCertificatePathTest1EE.crt data/certs/GoodCACert.crt data/crls/GoodCACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

import java.security.cert.PKIXParameters;
import java.util.HashSet;

public class AllCertificatesSamePolicyTest1_4 extends BaseValidTest
{
  public AllCertificatesSamePolicyTest1_4()
  {
    super (new String[] { "data/certs/ValidCertificatePathTest1EE.crt",
                          "data/certs/GoodCACert.crt" },
           new String[] { "data/crls/GoodCACRL.crl" });
  }

  protected void setupAdditionalParams (PKIXParameters params)
  {
    params.setExplicitPolicyRequired (true);
    HashSet policies = new HashSet();
    policies.add (NIST_TEST_POLICY_1);
    policies.add (NIST_TEST_POLICY_2);
    params.setInitialPolicies (policies);
  }
}
