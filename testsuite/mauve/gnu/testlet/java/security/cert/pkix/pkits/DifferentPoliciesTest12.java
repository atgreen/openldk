/* DifferentPoliciesTest12.java
   Copyright (C) 2004  Free Software Foundation, Inc.

   Distributed under the GPL; see the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseInvalidTest
// Files: data/certs/DifferentPoliciesTest12EE.crt data/certs/PoliciesP3CACert.crt data/crls/PoliciesP3CACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

import java.security.cert.PKIXParameters;

public class DifferentPoliciesTest12 extends BaseInvalidTest
{
  public DifferentPoliciesTest12()
  {
    super (new String[] { "data/certs/DifferentPoliciesTest12EE.crt",
                          "data/certs/PoliciesP3CACert.crt" },
           new String[] { "data/crls/PoliciesP3CACRL.crl" });
  }

  protected void setupAdditionalParams (PKIXParameters params)
  {
  }
}
