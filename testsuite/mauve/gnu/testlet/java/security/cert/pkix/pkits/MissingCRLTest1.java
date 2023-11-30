/* MissingCRLTest1.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseInvalidTest
// Files: data/certs/InvalidMissingCRLTest1EE.crt data/certs/NoCRLCACert.crt

package gnu.testlet.java.security.cert.pkix.pkits;

public class MissingCRLTest1 extends BaseInvalidTest
{
  public MissingCRLTest1()
  {
    super(new String[] { "data/certs/InvalidMissingCRLTest1EE.crt",
                         "data/certs/NoCRLCACert.crt" },
          new String[0]);
  }
}
