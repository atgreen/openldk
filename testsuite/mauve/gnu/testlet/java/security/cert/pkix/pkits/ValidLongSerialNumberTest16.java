/* ValidLongSerialNumberTest16.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseValidTest
// Files: data/certs/ValidLongSerialNumberTest16EE.crt data/certs/LongSerialNumberCACert.crt data/crls/LongSerialNumberCACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class ValidLongSerialNumberTest16 extends BaseValidTest
{
  public ValidLongSerialNumberTest16()
  {
    super(new String[] { "data/certs/ValidLongSerialNumberTest16EE.crt",
                         "data/certs/LongSerialNumberCACert.crt" },
          new String[] { "data/crls/LongSerialNumberCACRL.crl" });
  }
}
