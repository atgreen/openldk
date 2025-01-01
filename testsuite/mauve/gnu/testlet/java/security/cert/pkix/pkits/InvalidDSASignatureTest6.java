/* InvalidDSASignatureTest6.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseInvalidTest
// Files: data/certs/InvalidDSASignatureTest6EE.crt data/certs/DSACACert.crt data/crls/DSACACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class InvalidDSASignatureTest6 extends BaseInvalidTest
{
  public InvalidDSASignatureTest6()
  {
    super(new String[] { "data/certs/InvalidDSASignatureTest6EE.crt", "data/certs/DSACACert.crt" },
          new String[] { "data/crls/DSACACRL.crl" });
  }
}
