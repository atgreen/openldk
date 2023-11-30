/* Validpre2000UTCnotBeforeDateTest3.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseValidTest
// Files: data/certs/Validpre2000UTCnotBeforeDateTest3EE.crt data/certs/GoodCACert.crt data/crls/GoodCACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class Validpre2000UTCnotBeforeDateTest3 extends BaseValidTest
{
  public Validpre2000UTCnotBeforeDateTest3()
  {
    super(new String[] { "data/certs/Validpre2000UTCnotBeforeDateTest3EE.crt",
                         "data/certs/GoodCACert.crt" },
          new String[] { "data/crls/GoodCACRL.crl" });
  }
}
