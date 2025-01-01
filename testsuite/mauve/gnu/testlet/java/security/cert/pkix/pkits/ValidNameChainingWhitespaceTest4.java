/* ValidNameChainingWhitespaceTest4.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseValidTest
// Files: data/certs/ValidNameChainingWhitespaceTest4EE.crt data/certs/GoodCACert.crt data/crls/GoodCACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class ValidNameChainingWhitespaceTest4 extends BaseValidTest
{
  public ValidNameChainingWhitespaceTest4()
  {
    super(new String[] { "data/certs/ValidNameChainingWhitespaceTest4EE.crt", "data/certs/GoodCACert.crt" },
          new String[] { "data/crls/GoodCACRL.crl" });
  }
}
