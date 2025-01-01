/* InvalidCASignatureTest2.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseInvalidTest
// Files: data/certs/InvalidCASignatureTest2EE.crt data/certs/BadSignedCACert.crt data/crls/BadSignedCACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class InvalidCASignatureTest2 extends BaseInvalidTest
{
  public InvalidCASignatureTest2()
  {
    super(new String[] { "data/certs/InvalidCASignatureTest2EE.crt", "data/certs/BadSignedCACert.crt" },
          new String[] { "data/crls/BadSignedCACRL.crl" });
  }
}
