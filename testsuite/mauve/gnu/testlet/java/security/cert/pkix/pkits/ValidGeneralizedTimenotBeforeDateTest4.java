/* ValidGeneralizedTimenotBeforeDateTest4.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseValidTest
// Files: data/certs/ValidGeneralizedTimenotBeforeDateTest4EE.crt data/certs/GoodCACert.crt data/crls/GoodCACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class ValidGeneralizedTimenotBeforeDateTest4 extends BaseValidTest
{
  public ValidGeneralizedTimenotBeforeDateTest4()
  {
    super(new String[] { "data/certs/ValidGeneralizedTimenotBeforeDateTest4EE.crt", "data/certs/GoodCACert.crt" },
          new String[] { "data/crls/GoodCACRL.crl" });
  }
}
