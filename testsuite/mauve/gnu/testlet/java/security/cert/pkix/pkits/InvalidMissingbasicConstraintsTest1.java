/* InvalidMissingbasicConstraintsTest1.java
   Copyright (C) 2003  Free Software Foundation, Inc.

   Distributed under the GPL. See the file `COPYING' */

// Tags: JDK1.4
// Uses: BaseInvalidTest
// Files: data/certs/InvalidMissingbasicConstraintsTest1EE.crt data/certs/MissingbasicConstraintsCACert.crt data/crls/MissingbasicConstraintsCACRL.crl

package gnu.testlet.java.security.cert.pkix.pkits;

public class InvalidMissingbasicConstraintsTest1 extends BaseInvalidTest
{
  public InvalidMissingbasicConstraintsTest1()
  {
    super(new String[] { "data/certs/InvalidMissingbasicConstraintsTest1EE.crt",
                         "data/certs/MissingbasicConstraintsCACert.crt" },
          new String[] { "data/crls/MissingbasicConstraintsCACRL.crl" });
  }
}
