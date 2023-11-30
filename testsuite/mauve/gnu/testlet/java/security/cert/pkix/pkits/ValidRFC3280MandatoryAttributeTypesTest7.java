/* ValidRFC3280MandatoryAttributeTypesTest7.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseValidTest
// Files: data/certs/ValidRFC3280MandatoryAttributeTypesTest7EE.crt data/certs/RFC3280MandatoryAttributeTypesCACert.crt data/crls/RFC3280MandatoryAttributeTypesCACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class ValidRFC3280MandatoryAttributeTypesTest7 extends BaseValidTest
{
  public ValidRFC3280MandatoryAttributeTypesTest7()
  {
    super(new String[] { "data/certs/ValidRFC3280MandatoryAttributeTypesTest7EE.crt",
                         "data/certs/RFC3280MandatoryAttributeTypesCACert.crt" },
          new String[] { "data/crls/RFC3280MandatoryAttributeTypesCACRL.crl" });
  }
}
