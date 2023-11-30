/* ValidkeyUsageNotCriticalTest3.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseValidTest
// Files: data/certs/ValidkeyUsageNotCriticalTest3EE.crt data/certs/keyUsageNotCriticalCACert.crt data/crls/keyUsageNotCriticalCACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class ValidkeyUsageNotCriticalTest3 extends BaseValidTest
{
  public ValidkeyUsageNotCriticalTest3()
  {
    super(new String[] { "data/certs/ValidkeyUsageNotCriticalTest3EE.crt",
                         "data/certs/keyUsageNotCriticalCACert.crt" },
          new String[] { "data/crls/keyUsageNotCriticalCACRL.crl" });
  }
}
