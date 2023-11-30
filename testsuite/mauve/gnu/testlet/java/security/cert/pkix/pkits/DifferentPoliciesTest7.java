/* DifferentPoliciesTest7.java
   Copyright (C) 2004  Free Software Foundation, Inc.

   Distributed under the GPL; see the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseInvalidTest
// Files: data/certs/DifferentPoliciesTest7EE.crt data/certs/PoliciesP123subsubCAP12P1Cert.crt data/certs/PoliciesP123subCAP12Cert.crt data/certs/PoliciesP123CACert.crt data/crls/PoliciesP123subsubCAP12P1CRL.crl data/crls/PoliciesP123subCAP12CRL.crl data/crls/PoliciesP123CACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

import java.security.cert.PKIXParameters;
import java.util.Collections;

public class DifferentPoliciesTest7 extends BaseInvalidTest
{
  public DifferentPoliciesTest7()
  {
    super (new String[] { "data/certs/DifferentPoliciesTest7EE.crt",
                          "data/certs/PoliciesP123subsubCAP12P1Cert.crt",
                          "data/certs/PoliciesP123subCAP12Cert.crt",
                          "data/certs/PoliciesP123CACert.crt" },
           new String[] { "data/crls/PoliciesP123subsubCAP12P1CRL.crl",
                          "data/crls/PoliciesP123subCAP12CRL.crl",
                          "data/crls/PoliciesP123CACRL.crl" });
  }

  protected void setupAdditionalParams (PKIXParameters params)
  {
    params.setInitialPolicies (Collections.EMPTY_SET);
    params.setAnyPolicyInhibited (true);
  }
}
