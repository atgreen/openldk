/* ValidLongSerialNumberTest17.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseValidTest
// Files: data/certs/ValidLongSerialNumberTest17EE.crt data/certs/LongSerialNumberCACert.crt data/crls/LongSerialNumberCACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class ValidLongSerialNumberTest17 extends BaseValidTest
{
  public ValidLongSerialNumberTest17()
  {
    super(new String[] { "data/certs/ValidLongSerialNumberTest17EE.crt",
                         "data/certs/LongSerialNumberCACert.crt" },
          new String[] { "data/crls/LongSerialNumberCACRL.crl" });
  }
}
