/* testAll.java -- test all protocol versions and cipher suites.
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
// Uses: AbstractEngineTest SimpleX509KeyManager SimplePSKKeyManager SimpleX509TrustManager

package gnu.testlet.javax.net.ssl.SSLEngine;

import gnu.testlet.TestHarness;

import javax.net.ssl.SSLEngine;

/**
 * Test every protocol and cipher suite that we can.
 *
 * @author Casey Marshall (csm@gnu.org)
 */
public class TestHandshake
  extends AbstractEngineTest
{

  /* (non-Javadoc)
   * @see gnu.testlet.javax.net.ssl.SSLEngine.AbstractEngineTest#implTest(gnu.testlet.TestHarness)
   */
  protected void implTest(TestHarness harness)
  {
    String[] protocols;
    String[] suites;
    
    SSLEngine fake = context.createSSLEngine();
    protocols = fake.getSupportedProtocols();
    suites = fake.getSupportedCipherSuites();
    
    for (int i = 0; i < protocols.length; i++)
      {
        for (int j = 0; j < suites.length; j++)
          {
            // Skip static DH suites; we need a way to generate appropriate
            // certificates for these suites.
            if (suites[j].indexOf("DH_RSA") >= 0
                || suites[j].indexOf("DH_DSS") >= 0)
              continue;
            // Test these in GNU-specific test.
            if (suites[j].indexOf("PSK") >= 0
                || suites[j].indexOf("SRP") >= 0)
              continue;
            setupEngines(harness);
            clientEngine.setEnabledProtocols(new String[] { protocols[i] });
            clientEngine.setEnabledCipherSuites(new String[] { suites[j] });
            serverEngine.setEnabledCipherSuites(serverEngine.getSupportedCipherSuites());
            
            harness.checkPoint("SSLEngine/" + protocols[i] + "/"
                               + suites[j]);
            try
              {
                runHandshake();
                harness.check(true);
              }
            catch (Exception x)
              {
                harness.check(false);
                harness.debug(x);
              }
          }
      }
  }
}
