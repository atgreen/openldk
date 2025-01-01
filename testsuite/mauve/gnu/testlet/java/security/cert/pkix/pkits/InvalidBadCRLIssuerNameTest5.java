/* InvalidBadCRLIssuerNameTest5.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseInvalidTest
// Files: data/certs/InvalidBadCRLIssuerNameTest5EE.crt data/certs/BadCRLIssuerNameCACert.crt data/crls/BadCRLIssuerNameCACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class InvalidBadCRLIssuerNameTest5 extends BaseInvalidTest
{
  public InvalidBadCRLIssuerNameTest5()
  {
    super(new String[] { "data/certs/InvalidBadCRLIssuerNameTest5EE.crt",
                         "data/certs/BadCRLIssuerNameCACert.crt" },
          new String[] { "data/crls/BadCRLIssuerNameCACRL.crl" });
  }
}
