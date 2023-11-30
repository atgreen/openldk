/* InvalidUnknownCRLEntryExtensionTest8.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseInvalidTest
// Files: data/certs/InvalidUnknownCRLEntryExtensionTest8EE.crt data/certs/UnknownCRLEntryExtensionCACert.crt data/crls/UnknownCRLEntryExtensionCACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class InvalidUnknownCRLEntryExtensionTest8 extends BaseInvalidTest
{
  public InvalidUnknownCRLEntryExtensionTest8()
  {
    super(new String[] { "data/certs/InvalidUnknownCRLEntryExtensionTest8EE.crt",
                         "data/certs/UnknownCRLEntryExtensionCACert.crt" },
          new String[] { "data/crls/UnknownCRLEntryExtensionCACRL.crl" });
  }
}
