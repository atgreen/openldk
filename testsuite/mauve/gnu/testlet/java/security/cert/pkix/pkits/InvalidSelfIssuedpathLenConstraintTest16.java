/* InvalidSelfIssuedpathLenConstraintTest16.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseInvalidTest
// Files: data/certs/InvalidSelfIssuedpathLenConstraintTest16EE.crt data/certs/pathLenConstraint0subCA2Cert.crt data/certs/pathLenConstraint0SelfIssuedCACert.crt data/certs/pathLenConstraint0CACert.crt data/crls/pathLenConstraint0CACRL.crl data/crls/pathLenConstraint0subCA2CRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class InvalidSelfIssuedpathLenConstraintTest16 extends BaseInvalidTest
{
  public InvalidSelfIssuedpathLenConstraintTest16()
  {
    super(new String[] { "data/certs/InvalidSelfIssuedpathLenConstraintTest16EE.crt",
                         "data/certs/pathLenConstraint0subCA2Cert.crt",
                         "data/certs/pathLenConstraint0SelfIssuedCACert.crt",
                         "data/certs/pathLenConstraint0CACert.crt" },
          new String[] { "data/crls/pathLenConstraint0CACRL.crl",
                         "data/crls/pathLenConstraint0subCA2CRL.crl" });
  }
}
