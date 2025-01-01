/* ValidpathLenConstraintTest7.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseValidTest
// Files: data/certs/ValidpathLenConstraintTest7EE.crt data/certs/pathLenConstraint0CACert.crt data/crls/pathLenConstraint0CACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class ValidpathLenConstraintTest7 extends BaseValidTest
{
  public ValidpathLenConstraintTest7()
  {
    super(new String[] { "data/certs/ValidpathLenConstraintTest7EE.crt",
                         "data/certs/pathLenConstraint0CACert.crt" },
          new String[] { "data/crls/pathLenConstraint0CACRL.crl" });
  }
}
