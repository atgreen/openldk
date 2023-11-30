/* InvalidBasicSelfIssuedNewWithOldTest5.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseInvalidTest
// Files: data/certs/InvalidBasicSelfIssuedNewWithOldTest5EE.crt data/certs/BasicSelfIssuedOldKeyNewWithOldCACert.crt data/certs/BasicSelfIssuedOldKeyCACert.crt data/crls/BasicSelfIssuedOldKeyCACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class InvalidBasicSelfIssuedNewWithOldTest5 extends BaseInvalidTest
{
  public InvalidBasicSelfIssuedNewWithOldTest5()
  {
    super(new String[] { "data/certs/InvalidBasicSelfIssuedNewWithOldTest5EE.crt",
                         "data/certs/BasicSelfIssuedOldKeyNewWithOldCACert.crt",
                         "data/certs/BasicSelfIssuedOldKeyCACert.crt" },
          new String[] { "data/crls/BasicSelfIssuedOldKeyCACRL.crl" });
  }
}
