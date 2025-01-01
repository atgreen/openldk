/* TestNoProtocols.java -- test handshake failure with no common protocols.
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

// Tags: JDK1.5 JSSE

package gnu.testlet.javax.net.ssl.SSLEngine;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Test a connection between SSLEngines that don't have any protocols in
 * common (that is, test that the handshake fails).
 *
 * @author Casey Marshall (csm@gnu.org)
 */
public class TestNoProtocols extends AbstractEngineTest
{

  /* (non-Javadoc)
   * @see gnu.testlet.javax.net.ssl.SSLEngine.AbstractEngineTest#implTest(gnu.testlet.TestHarness)
   */
  protected void implTest(TestHarness harness)
  {
    setupEngines(harness);
    clientEngine.setEnabledProtocols(new String[] { "SSLv3" });
    serverEngine.setEnabledProtocols(new String[] { "TLSv1", "TLSv1.1" });

    harness.checkPoint("SSLEngine/protcols");
    try
      {
        runHandshake();
        harness.fail("SSLEngine/protocols");
      }
    catch (Exception e)
      {
        harness.check(true);
      }
  }
}
