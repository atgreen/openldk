/* TestOfPR25981.java
   Copyright (C) 2006 Free Software Foundation, Inc.
This file is part of Mauve.

Mauve is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Mauve is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Mauve; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

*/

// Tags: GNU-CRYPTO JDK1.4

package gnu.testlet.gnu.javax.crypto.jce;

import gnu.javax.crypto.jce.GnuCrypto;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.security.Security;

import javax.crypto.KeyGenerator;

/**
 * Regression test for PR Classpath/25981
 */
public class TestOfPR25981 implements Testlet
{
  /* (non-Javadoc)
   * @see gnu.testlet.Testlet#test(gnu.testlet.TestHarness)
   */
  public void test(TestHarness harness)
  {
    harness.checkPoint("TestOfPR25981");
    setUp();
    try
    {
      KeyGenerator kgen = KeyGenerator.getInstance("AES");
      harness.check(kgen != null, "AES KeyGenerator MUST be available");
    }
    catch (Exception x)
    {
      harness.debug(x);
      harness.fail("test(): " + String.valueOf(x));
    }
  }

  private void setUp()
  {
    Security.addProvider(new GnuCrypto());
  }
}
