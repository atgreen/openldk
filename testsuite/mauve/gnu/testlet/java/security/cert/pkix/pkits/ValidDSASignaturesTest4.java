/* ValidDSASignaturesTest4.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseValidTest
// Files: data/certs/ValidDSASignaturesTest4EE.crt data/certs/DSACACert.crt data/crls/DSACACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class ValidDSASignaturesTest4 extends BaseValidTest
{
  public ValidDSASignaturesTest4()
  {
    super(new String[] { "data/certs/ValidDSASignaturesTest4EE.crt", "data/certs/DSACACert.crt" },
          new String[] { "data/crls/DSACACRL.crl" });
  }
}
