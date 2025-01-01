/* ValidbasicConstraintsNotCriticalTest4.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseValidTest
// Files: data/certs/ValidbasicConstraintsNotCriticalTest4EE.crt data/certs/basicConstraintsNotCriticalCACert.crt data/crls/basicConstraintsNotCriticalCACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class ValidbasicConstraintsNotCriticalTest4 extends BaseValidTest
{
  public ValidbasicConstraintsNotCriticalTest4()
  {
    super(new String[] { "data/certs/ValidbasicConstraintsNotCriticalTest4EE.crt",
                         "data/certs/basicConstraintsNotCriticalCACert.crt" },
          new String[] { "data/crls/basicConstraintsNotCriticalCACRL.crl" });
  }
}
