/* ValidBasicSelfIssuedOldWithNewTest1.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseValidTest
// Files: data/certs/ValidBasicSelfIssuedOldWithNewTest1EE.crt data/certs/BasicSelfIssuedNewKeyOldWithNewCACert.crt data/certs/BasicSelfIssuedNewKeyCACert.crt data/crls/BasicSelfIssuedNewKeyCACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class ValidBasicSelfIssuedOldWithNewTest1 extends BaseValidTest
{
  public ValidBasicSelfIssuedOldWithNewTest1()
  {
    super(new String[] { "data/certs/ValidBasicSelfIssuedOldWithNewTest1EE.crt",
                         "data/certs/BasicSelfIssuedNewKeyOldWithNewCACert.crt",
                         "data/certs/BasicSelfIssuedNewKeyCACert.crt" },
          new String[] { "data/crls/BasicSelfIssuedNewKeyCACRL.crl" });
  }
}
