/* InvalidcAFalseTest2.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseInvalidTest
// Files: data/certs/InvalidcAFalseTest2EE.crt data/certs/basicConstraintsCriticalcAFalseCACert.crt data/crls/basicConstraintsCriticalcAFalseCACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class InvalidcAFalseTest2 extends BaseInvalidTest
{
  public InvalidcAFalseTest2()
  {
    super(new String[] { "data/certs/InvalidcAFalseTest2EE.crt",
                         "data/certs/basicConstraintsCriticalcAFalseCACert.crt" },
          new String[] { "data/crls/basicConstraintsCriticalcAFalseCACRL.crl" });
  }
}
