/* ValidUTF8StringCaseInsensitiveMatchTest11.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseValidTest
// Files: data/certs/ValidUTF8StringCaseInsensitiveMatchTest11EE.crt data/certs/UTF8StringCaseInsensitiveMatchCACert.crt data/crls/UTF8StringCaseInsensitiveMatchCACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class ValidUTF8StringCaseInsensitiveMatchTest11 extends BaseValidTest
{
  public ValidUTF8StringCaseInsensitiveMatchTest11()
  {
    super(new String[] { "data/certs/ValidUTF8StringCaseInsensitiveMatchTest11EE.crt",
                         "data/certs/UTF8StringCaseInsensitiveMatchCACert.crt" },
          new String[] { "data/crls/UTF8StringCaseInsensitiveMatchCACRL.crl" });
  }
}
