/* ValidRolloverfromPrintableStringtoUTF8StringTest10.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseValidTest
// Files: data/certs/ValidRolloverfromPrintableStringtoUTF8StringTest10EE.crt data/certs/RolloverfromPrintableStringtoUTF8StringCACert.crt data/crls/RolloverfromPrintableStringtoUTF8StringCACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class ValidRolloverfromPrintableStringtoUTF8StringTest10 extends BaseValidTest
{
  public ValidRolloverfromPrintableStringtoUTF8StringTest10()
  {
    super(new String[] { "data/certs/ValidRolloverfromPrintableStringtoUTF8StringTest10EE.crt",
                         "data/certs/RolloverfromPrintableStringtoUTF8StringCACert.crt" },
          new String[] { "data/crls/RolloverfromPrintableStringtoUTF8StringCACRL.crl" });
  }
}
