/* InvalidRevokedEETest3.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseInvalidTest
// Files: data/certs/InvalidRevokedEETest3EE.crt data/certs/GoodCACert.crt data/crls/GoodCACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class InvalidRevokedEETest3 extends BaseInvalidTest
{
  public InvalidRevokedEETest3()
  {
    super(new String[] { "data/certs/InvalidRevokedEETest3EE.crt",
                         "data/certs/GoodCACert.crt" },
          new String[] { "data/crls/GoodCACRL.crl" });
  }
}
