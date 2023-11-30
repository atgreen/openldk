/* ValidNegativeSerialNumberTest14.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseValidTest
// Files: data/certs/ValidNegativeSerialNumberTest14EE.crt data/certs/NegativeSerialNumberCACert.crt data/crls/NegativeSerialNumberCACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class ValidNegativeSerialNumberTest14 extends BaseValidTest
{
  public ValidNegativeSerialNumberTest14()
  {
    super(new String[] { "data/certs/ValidNegativeSerialNumberTest14EE.crt",
                         "data/certs/NegativeSerialNumberCACert.crt" },
          new String[] { "data/crls/NegativeSerialNumberCACRL.crl" });
  }
}
