/* TestOfPRNGFactory.java -- 
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

package gnu.testlet.gnu.javax.crypto.prng;

import gnu.java.security.prng.IRandom;
import gnu.javax.crypto.prng.PRNGFactory;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import java.util.Iterator;

/**
 * Conformance tests for the PRNGFactory implementation.
 */
public class TestOfPRNGFactory implements Testlet
{
  public void test(TestHarness harness)
  {
    harness.checkPoint("TestOfPRNGFactory");
    String prng;
    IRandom algorithm;
    for (Iterator it = PRNGFactory.getNames().iterator(); it.hasNext();)
      {
        prng = (String) it.next();
        try
          {
            algorithm = null;
            algorithm = PRNGFactory.getInstance(prng);
            harness.check(algorithm != null, "getInstance("
                                             + String.valueOf(prng) + ")");
          }
        catch (InternalError x)
          {
            harness.fail("TestOfPRNGFactory.getInstance("
                         + String.valueOf(prng) + ")");
          }
      }
  }
}