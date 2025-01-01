/* InvalidBadCRLSignatureTest4.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseInvalidTest
// Files: data/certs/InvalidBadCRLSignatureTest4EE.crt data/certs/BadCRLSignatureCACert.crt data/crls/BadCRLSignatureCACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class InvalidBadCRLSignatureTest4 extends BaseInvalidTest
{
  public InvalidBadCRLSignatureTest4()
  {
    super(new String[] { "data/certs/InvalidBadCRLSignatureTest4EE.crt",
                         "data/certs/BadCRLSignatureCACert.crt" },
          new String[] { "data/crls/BadCRLSignatureCACRL.crl" });
  }
}
