/* InvalidkeyUsageCriticalcRLSignFalseTest4.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseInvalidTest
// Files: data/certs/InvalidkeyUsageCriticalcRLSignFalseTest4EE.crt data/certs/keyUsageCriticalcRLSignFalseCACert.crt data/crls/keyUsageCriticalcRLSignFalseCACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class InvalidkeyUsageCriticalcRLSignFalseTest4 extends BaseInvalidTest
{
  public InvalidkeyUsageCriticalcRLSignFalseTest4()
  {
    super(new String[] { "data/certs/InvalidkeyUsageCriticalcRLSignFalseTest4EE.crt",
                         "data/certs/keyUsageCriticalcRLSignFalseCACert.crt" },
          new String[] { "data/crls/keyUsageCriticalcRLSignFalseCACRL.crl" });
  }
}
