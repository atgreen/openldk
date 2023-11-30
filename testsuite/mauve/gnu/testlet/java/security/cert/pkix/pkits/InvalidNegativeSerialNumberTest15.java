/* InvalidNegativeSerialNumberTest15.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseInvalidTest
// Files: data/certs/InvalidNegativeSerialNumberTest15EE.crt data/certs/NegativeSerialNumberCACert.crt data/crls/NegativeSerialNumberCACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class InvalidNegativeSerialNumberTest15 extends BaseInvalidTest
{
  public InvalidNegativeSerialNumberTest15()
  {
    super(new String[] { "data/certs/InvalidNegativeSerialNumberTest15EE.crt",
                         "data/certs/NegativeSerialNumberCACert.crt" },
          new String[] { "data/crls/NegativeSerialNumberCACRL.crl" });
  }
}
