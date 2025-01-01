/* InvalidBasicSelfIssuedOldWithNewTest2.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseInvalidTest
// Files: data/certs/InvalidBasicSelfIssuedOldWithNewTest2EE.crt data/certs/BasicSelfIssuedNewKeyOldWithNewCACert.crt data/certs/BasicSelfIssuedNewKeyCACert.crt data/crls/BasicSelfIssuedNewKeyCACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class InvalidBasicSelfIssuedOldWithNewTest2 extends BaseInvalidTest
{
  public InvalidBasicSelfIssuedOldWithNewTest2()
  {
    super(new String[] { "data/certs/InvalidBasicSelfIssuedOldWithNewTest2EE.crt",
                         "data/certs/BasicSelfIssuedNewKeyOldWithNewCACert.crt",
                         "data/certs/BasicSelfIssuedNewKeyCACert.crt" },
          new String[] { "data/crls/BasicSelfIssuedNewKeyCACRL.crl" });
  }
}
