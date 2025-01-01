/* ValidUTF8StringEncodedNamesTest9.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseValidTest
// Files: data/certs/ValidUTF8StringEncodedNamesTest9EE.crt data/certs/UTF8StringEncodedNamesCACert.crt data/crls/UTF8StringEncodedNamesCACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class ValidUTF8StringEncodedNamesTest9 extends BaseValidTest
{
  public ValidUTF8StringEncodedNamesTest9()
  {
    super(new String[] { "data/certs/ValidUTF8StringEncodedNamesTest9EE.crt",
                         "data/certs/UTF8StringEncodedNamesCACert.crt" },
          new String[] { "data/crls/UTF8StringEncodedNamesCACRL.crl" });
  }
}
