/* ValidNameChainingWhitespaceTest3.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseValidTest
// Files: data/certs/ValidNameChainingWhitespaceTest3EE.crt data/certs/GoodCACert.crt data/crls/GoodCACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class ValidNameChainingWhitespaceTest3 extends BaseValidTest
{
  public ValidNameChainingWhitespaceTest3()
  {
    super(new String[] { "data/certs/ValidNameChainingWhitespaceTest3EE.crt", "data/certs/GoodCACert.crt" },
          new String[] { "data/crls/GoodCACRL.crl" });
  }
}
