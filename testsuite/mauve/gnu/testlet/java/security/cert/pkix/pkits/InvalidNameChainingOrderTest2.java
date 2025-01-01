/* InvalidNameChainingOrderTest2.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseInvalidTest
// Files: data/certs/InvalidNameChainingOrderTest2EE.crt data/certs/NameOrderingCACert.crt data/crls/NameOrderCACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class InvalidNameChainingOrderTest2 extends BaseInvalidTest
{
  public InvalidNameChainingOrderTest2()
  {
    super(new String[] { "data/certs/InvalidNameChainingOrderTest2EE.crt", "data/certs/NameOrderingCACert.crt" },
          new String[] { "data/crls/NameOrderCACRL.crl" });
  }
}
