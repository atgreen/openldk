/* InvalidkeyUsageNotCriticalkeyCertSignFalseTest2.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseInvalidTest
// Files: data/certs/InvalidkeyUsageNotCriticalkeyCertSignFalseTest2EE.crt data/certs/keyUsageNotCriticalkeyCertSignFalseCACert.crt data/crls/keyUsageNotCriticalkeyCertSignFalseCACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class InvalidkeyUsageNotCriticalkeyCertSignFalseTest2 extends BaseInvalidTest
{
  public InvalidkeyUsageNotCriticalkeyCertSignFalseTest2()
  {
    super(new String[] { "data/certs/InvalidkeyUsageNotCriticalkeyCertSignFalseTest2EE.crt",
                         "data/certs/keyUsageNotCriticalkeyCertSignFalseCACert.crt" },
          new String[] { "data/crls/keyUsageNotCriticalkeyCertSignFalseCACRL.crl" });
  }
}
