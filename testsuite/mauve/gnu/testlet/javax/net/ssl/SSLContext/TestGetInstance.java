/* testGetInstance.java -- test if SSLContext.getInstance works.
   Copyright (C) 2006  Casey Marshall <csm@gnu.org>

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

// Tags: JDK1.5

package gnu.testlet.javax.net.ssl.SSLContext;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.security.NoSuchAlgorithmException;

import javax.net.ssl.SSLContext;

/**
 * @author Casey Marshall (csm@gnu.org)
 */
public class TestGetInstance implements Testlet
{

  /* (non-Javadoc)
   * @see gnu.testlet.Testlet#test(gnu.testlet.TestHarness)
   */
  public void test(TestHarness harness)
  {
    SSLContext context = null;
    harness.checkPoint("SSLContext.getInstance(\"SSL\")");
    try
      {
        context = SSLContext.getInstance("SSL");
        harness.check(context != null);
      }
    catch (NoSuchAlgorithmException nsae)
      {
        harness.fail("SSLContext.getInstance(\"SSL\")");
        harness.debug(nsae);
      }
  }
}
