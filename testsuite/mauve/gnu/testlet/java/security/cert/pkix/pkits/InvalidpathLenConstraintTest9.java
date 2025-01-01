/* InvalidpathLenConstraintTest9.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseInvalidTest
// Files: data/certs/InvalidpathLenConstraintTest9EE.crt data/certs/pathLenConstraint6subsubCA00Cert.crt data/certs/pathLenConstraint6subCA0Cert.crt data/certs/pathLenConstraint6CACert.crt data/crls/pathLenConstraint6subsubCA00CRL.crl data/crls/pathLenConstraint6subCA0CRL.crl data/crls/pathLenConstraint6CACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class InvalidpathLenConstraintTest9 extends BaseInvalidTest
{
  public InvalidpathLenConstraintTest9()
  {
    super(new String[] { "data/certs/InvalidpathLenConstraintTest9EE.crt",
                         "data/certs/pathLenConstraint6subsubCA00Cert.crt",
                         "data/certs/pathLenConstraint6subCA0Cert.crt",
                         "data/certs/pathLenConstraint6CACert.crt" },
          new String[] { "data/crls/pathLenConstraint6subsubCA00CRL.crl",
                         "data/crls/pathLenConstraint6subCA0CRL.crl",
                         "data/crls/pathLenConstraint6CACRL.crl" });
  }
}
