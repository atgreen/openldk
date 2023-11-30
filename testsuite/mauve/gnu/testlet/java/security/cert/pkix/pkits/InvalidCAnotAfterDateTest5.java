/* InvalidCAnotAfterDateTest5.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseInvalidTest
// Files: data/certs/InvalidCAnotAfterDateTest5EE.crt data/certs/BadnotAfterDateCACert.crt data/crls/BadnotAfterDateCACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class InvalidCAnotAfterDateTest5 extends BaseInvalidTest
{
  public InvalidCAnotAfterDateTest5()
  {
    super(new String[] { "data/certs/InvalidCAnotAfterDateTest5EE.crt",
                         "data/certs/BadnotAfterDateCACert.crt" },
          new String[] { "data/crls/BadnotAfterDateCACRL.crl" });
  }
}
