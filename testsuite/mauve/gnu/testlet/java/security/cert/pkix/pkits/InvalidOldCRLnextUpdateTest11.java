/* InvalidOldCRLnextUpdateTest11.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseInvalidTest
// Files: data/certs/InvalidOldCRLnextUpdateTest11EE.crt data/certs/OldCRLnextUpdateCACert.crt data/crls/OldCRLnextUpdateCACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class InvalidOldCRLnextUpdateTest11 extends BaseInvalidTest
{
  public InvalidOldCRLnextUpdateTest11()
  {
    super(new String[] { "data/certs/InvalidOldCRLnextUpdateTest11EE.crt",
                         "data/certs/OldCRLnextUpdateCACert.crt" },
          new String[] { "data/crls/OldCRLnextUpdateCACRL.crl" });
  }
}
