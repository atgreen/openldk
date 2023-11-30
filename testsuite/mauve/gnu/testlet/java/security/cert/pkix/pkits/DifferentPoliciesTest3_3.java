/* DifferentPoliciesTest3_3.java
   Copyright (C) 2004  Free Software Foundation, Inc.

   Distributed under the GPL; see the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseInvalidTest
// Files: data/certs/DifferentPoliciesTest3EE.crt data/certs/PoliciesP2subCACert.crt data/certs/GoodCACert.crt data/crls/PoliciesP2subCACRL.crl data/crls/GoodCACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

import java.security.cert.PKIXParameters;
import java.util.HashSet;

public class DifferentPoliciesTest3_3 extends BaseInvalidTest
{
  public DifferentPoliciesTest3_3()
  {
    super (new String[] { "data/certs/DifferentPoliciesTest3EE.crt",
                          "data/certs/PoliciesP2subCACert.crt",
                          "data/certs/GoodCACert.crt" },
           new String[] { "data/crls/PoliciesP2subCACRL.crl",
                          "data/crls/GoodCACRL.crl" });
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
