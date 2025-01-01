/* InvalidBasicSelfIssuedCRLSigningKeyTest7.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseInvalidTest
// Files: data/certs/InvalidBasicSelfIssuedCRLSigningKeyTest7EE.crt data/certs/BasicSelfIssuedCRLSigningKeyCACert.crt data/crls/BasicSelfIssuedCRLSigningKeyCACRL.crl data/certs/BasicSelfIssuedCRLSigningKeyCRLCert.crt

package gnu.testlet.java.security.cert.pkix.pkits;

public class InvalidBasicSelfIssuedCRLSigningKeyTest7 extends BaseInvalidTest
{
  public InvalidBasicSelfIssuedCRLSigningKeyTest7()
  {
    super(new String[] { "data/certs/InvalidBasicSelfIssuedCRLSigningKeyTest7EE.crt",
                         "data/certs/BasicSelfIssuedCRLSigningKeyCACert.crt" },
          new String[] { "data/crls/BasicSelfIssuedCRLSigningKeyCACRL.crl" },
          new String[] { "data/certs/BasicSelfIssuedCRLSigningKeyCRLCert.crt" });
  }
}
