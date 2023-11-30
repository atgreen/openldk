/* InvalidpathLenConstraintTest11.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseInvalidTest
// Files: data/certs/InvalidpathLenConstraintTest11EE.crt data/certs/pathLenConstraint6subsubsubCA11XCert.crt data/certs/pathLenConstraint6subsubCA11Cert.crt data/certs/pathLenConstraint6subCA1Cert.crt data/certs/pathLenConstraint6CACert.crt data/crls/pathLenConstraint6subsubsubCA11XCRL.crl data/crls/pathLenConstraint6subsubCA11CRL.crl data/crls/pathLenConstraint6subCA1CRL.crl data/crls/pathLenConstraint6CACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class InvalidpathLenConstraintTest11 extends BaseInvalidTest
{
  public InvalidpathLenConstraintTest11()
  {
    super(new String[] { "data/certs/InvalidpathLenConstraintTest11EE.crt",
                         "data/certs/pathLenConstraint6subsubsubCA11XCert.crt",
                         "data/certs/pathLenConstraint6subsubCA11Cert.crt",
                         "data/certs/pathLenConstraint6subCA1Cert.crt",
                         "data/certs/pathLenConstraint6CACert.crt" },
          new String[] { "data/crls/pathLenConstraint6subsubsubCA11XCRL.crl",
                         "data/crls/pathLenConstraint6subsubCA11CRL.crl",
                         "data/crls/pathLenConstraint6subCA1CRL.crl",
                         "data/crls/pathLenConstraint6CACRL.crl" });
  }
}
