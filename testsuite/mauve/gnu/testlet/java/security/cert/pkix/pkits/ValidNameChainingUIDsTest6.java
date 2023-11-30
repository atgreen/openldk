/* ValidNameChainingUIDsTest6.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseValidTest
// Files: data/certs/ValidNameUIDsTest6EE.crt data/certs/UIDCACert.crt data/crls/UIDCACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class ValidNameChainingUIDsTest6 extends BaseValidTest
{
  public ValidNameChainingUIDsTest6()
  {
    super(new String[] { "data/certs/ValidNameUIDsTest6EE.crt", "data/certs/UIDCACert.crt" },
          new String[] { "data/crls/UIDCACRL.crl" });
  }
}
