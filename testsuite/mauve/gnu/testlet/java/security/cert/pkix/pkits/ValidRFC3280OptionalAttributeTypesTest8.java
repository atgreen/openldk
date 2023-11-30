/* ValidRFC3280OptionalAttributeTypesTest8.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseValidTest
// Files: data/certs/ValidRFC3280OptionalAttributeTypesTest8EE.crt data/certs/RFC3280OptionalAttributeTypesCACert.crt data/crls/RFC3280OptionalAttributeTypesCACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class ValidRFC3280OptionalAttributeTypesTest8 extends BaseValidTest
{
  public ValidRFC3280OptionalAttributeTypesTest8()
  {
    super(new String[] { "data/certs/ValidRFC3280OptionalAttributeTypesTest8EE.crt",
                         "data/certs/RFC3280OptionalAttributeTypesCACert.crt" },
          new String[] { "data/crls/RFC3280OptionalAttributeTypesCACRL.crl" });
  }
}
