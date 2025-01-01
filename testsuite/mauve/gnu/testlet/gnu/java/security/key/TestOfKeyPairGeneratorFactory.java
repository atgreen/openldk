/* TestOfKeyPairGeneratorFactory.java -- 
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

package gnu.testlet.gnu.java.security.key;

import gnu.java.security.key.IKeyPairGenerator;
import gnu.java.security.key.KeyPairGeneratorFactory;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import java.util.Iterator;

/**
 * Conformance tests for the KeyPairGeneratorFactory implementation.
 */
public class TestOfKeyPairGeneratorFactory implements Testlet
{
  public void test(TestHarness harness)
  {
    harness.checkPoint("TestOfKeyPairGeneratorFactory");
    String kpg;
    IKeyPairGenerator algorithm;
    for (Iterator it = KeyPairGeneratorFactory.getNames().iterator(); it.hasNext();)
      {
        kpg = (String) it.next();
        try
          {
            algorithm = null;
            algorithm = KeyPairGeneratorFactory.getInstance(kpg);
            harness.check(algorithm != null, "getInstance("
                                             + String.valueOf(kpg) + ")");
          }
        catch (RuntimeException x)
          {
            harness.debug(x);
            harness.fail("TestOfKeyPairGeneratorFactory.getInstance("
                         + String.valueOf(kpg) + ")");
          }
      }
  }
}