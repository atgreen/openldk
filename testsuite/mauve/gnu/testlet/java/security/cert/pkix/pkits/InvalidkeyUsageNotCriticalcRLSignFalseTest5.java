/* InvalidkeyUsageNotCriticalcRLSignFalseTest5.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseInvalidTest
// Files: data/certs/InvalidkeyUsageNotCriticalcRLSignFalseTest5EE.crt data/certs/keyUsageNotCriticalcRLSignFalseCACert.crt data/crls/keyUsageNotCriticalcRLSignFalseCACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class InvalidkeyUsageNotCriticalcRLSignFalseTest5 extends BaseInvalidTest
{
  public InvalidkeyUsageNotCriticalcRLSignFalseTest5()
  {
    super(new String[] { "data/certs/InvalidkeyUsageNotCriticalcRLSignFalseTest5EE.crt",
                         "data/certs/keyUsageNotCriticalcRLSignFalseCACert.crt" },
          new String[] { "data/crls/keyUsageNotCriticalcRLSignFalseCACRL.crl" });
  }
}
