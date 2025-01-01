/* InvalidUnknownCRLExtensionTest9.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseInvalidTest
// Files: data/certs/InvalidUnknownCRLExtensionTest9EE.crt data/certs/UnknownCRLExtensionCACert.crt data/crls/UnknownCRLExtensionCACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class InvalidUnknownCRLExtensionTest9 extends BaseInvalidTest
{
  public InvalidUnknownCRLExtensionTest9()
  {
    super(new String[] { "data/certs/InvalidUnknownCRLExtensionTest9EE.crt",
                         "data/certs/UnknownCRLExtensionCACert.crt" },
          new String[] { "data/crls/UnknownCRLExtensionCACRL.crl" });
  }
}
