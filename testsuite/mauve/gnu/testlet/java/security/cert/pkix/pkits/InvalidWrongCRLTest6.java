/* InvalidWrongCRLTest6.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseInvalidTest
// Files: data/certs/InvalidWrongCRLTest6EE.crt data/certs/WrongCRLCACert.crt data/crls/WrongCRLCACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class InvalidWrongCRLTest6 extends BaseInvalidTest
{
  public InvalidWrongCRLTest6()
  {
    super(new String[] { "data/certs/InvalidWrongCRLTest6EE.crt",
                         "data/certs/WrongCRLCACert.crt" },
          new String[] { "data/crls/WrongCRLCACRL.crl" });
  }
}
