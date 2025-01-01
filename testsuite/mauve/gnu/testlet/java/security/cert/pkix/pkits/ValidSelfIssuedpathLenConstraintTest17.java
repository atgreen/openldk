/* ValidSelfIssuedpathLenConstraintTest17.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseValidTest
// Files: data/certs/ValidSelfIssuedpathLenConstraintTest17EE.crt data/certs/pathLenConstraint1SelfIssuedsubCACert.crt data/certs/pathLenConstraint1subCACert.crt data/certs/pathLenConstraint1SelfIssuedCACert.crt data/certs/pathLenConstraint1CACert.crt data/crls/pathLenConstraint1CACRL.crl data/crls/pathLenConstraint1subCACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class ValidSelfIssuedpathLenConstraintTest17 extends BaseValidTest
{
  public ValidSelfIssuedpathLenConstraintTest17()
  {
    super(new String[] { "data/certs/ValidSelfIssuedpathLenConstraintTest17EE.crt",
                         "data/certs/pathLenConstraint1SelfIssuedsubCACert.crt",
                         "data/certs/pathLenConstraint1subCACert.crt",
                         "data/certs/pathLenConstraint1SelfIssuedCACert.crt",
                         "data/certs/pathLenConstraint1CACert.crt" },
          new String[] { "data/crls/pathLenConstraint1CACRL.crl",
                         "data/crls/pathLenConstraint1subCACRL.crl" });
  }
}
