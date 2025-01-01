/* InvalidcAFalseTest3.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseInvalidTest
// Files: data/certs/InvalidcAFalseTest3EE.crt data/certs/basicConstraintsNotCriticalcAFalseCACert.crt data/crls/basicConstraintsNotCriticalcAFalseCACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class InvalidcAFalseTest3 extends BaseInvalidTest
{
  public InvalidcAFalseTest3()
  {
    super(new String[] { "data/certs/InvalidcAFalseTest3EE.crt",
                         "data/certs/basicConstraintsNotCriticalcAFalseCACert.crt" },
          new String[] { "data/crls/basicConstraintsNotCriticalcAFalseCACRL.crl" });
  }
}
