/* InvalidpathLenConstraintTest5.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseInvalidTest
// Files: data/certs/InvalidpathLenConstraintTest5EE.crt data/certs/pathLenConstraint0subCACert.crt data/certs/pathLenConstraint0CACert.crt data/crls/pathLenConstraint0subCACRL.crl data/crls/pathLenConstraint0CACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class InvalidpathLenConstraintTest5 extends BaseInvalidTest
{
  public InvalidpathLenConstraintTest5()
  {
    super(new String[] { "data/certs/InvalidpathLenConstraintTest5EE.crt",
                         "data/certs/pathLenConstraint0subCACert.crt",
                         "data/certs/pathLenConstraint0CACert.crt" },
          new String[] { "data/crls/pathLenConstraint0subCACRL.crl",
                         "data/crls/pathLenConstraint0CACRL.crl" });
  }
}
