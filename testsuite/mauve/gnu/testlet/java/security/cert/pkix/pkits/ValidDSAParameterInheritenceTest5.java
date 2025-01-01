/* ValidDSAParameterInheritenceTest5.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseValidTest
// Files: data/certs/ValidDSAParameterInheritanceTest5EE.crt data/certs/DSAParametersInheritedCACert.crt data/certs/DSACACert.crt data/crls/DSACACRL.crl data/crls/DSAParametersInheritedCACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class ValidDSAParameterInheritenceTest5 extends BaseValidTest
{
  public ValidDSAParameterInheritenceTest5()
  {
    super(new String[] { "data/certs/ValidDSAParameterInheritanceTest5EE.crt", "data/certs/DSAParametersInheritedCACert.crt",
                         "data/certs/DSACACert.crt" },
          new String[] { "data/crls/DSACACRL.crl", "data/crls/DSAParametersInheritedCACRL.crl" });
  }
}
