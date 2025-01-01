/* DifferentPoliciesTest5.java
   Copyright (C) 2004  Free Software Foundation, Inc.

   Distributed under the GPL; see the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseInvalidTest
// Files: data/certs/DifferentPoliciesTest5EE.crt data/certs/PoliciesP2subCA2Cert.crt data/certs/GoodCACert.crt data/crls/PoliciesP2subCA2CRL.crl data/crls/GoodCACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

import java.security.cert.PKIXParameters;
import java.util.Collections;

public class DifferentPoliciesTest5 extends BaseInvalidTest
{
  public DifferentPoliciesTest5()
  {
    super (new String[] { "data/certs/DifferentPoliciesTest5EE.crt",
                          "data/certs/PoliciesP2subCA2Cert.crt",
                          "data/certs/GoodCACert.crt" },
           new String[] { "data/crls/PoliciesP2subCA2CRL.crl",
                          "data/crls/GoodCACRL.crl" });
  }

  protected void setupAdditionalParams (PKIXParameters params)
  {
    params.setInitialPolicies (Collections.EMPTY_SET);
    params.setAnyPolicyInhibited (true);
  }
}
