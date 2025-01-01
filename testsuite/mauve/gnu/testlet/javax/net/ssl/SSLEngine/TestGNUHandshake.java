/* TestGNUHandshake.java -- test GNU-supported cipher suites.
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

// Tags: GNU JESSIE
// Uses: AbstractEngineTest SimpleX509KeyManager SimplePSKKeyManager SimpleX509TrustManager

package gnu.testlet.javax.net.ssl.SSLEngine;

import gnu.testlet.TestHarness;

import java.security.SecureRandom;
import java.security.Security;

import javax.net.ssl.KeyManager;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLEngine;
import javax.net.ssl.TrustManager;

/**
 * @author Casey Marshall (csm@gnu.org)
 */
public class TestGNUHandshake
  extends AbstractEngineTest
{
  protected boolean setup(TestHarness harness)
  {
    try
      {
        harness.checkPoint("SSLContext.getInstance");
        context = SSLContext.getInstance("SSL");
        context.init(new KeyManager[] { new SimpleX509KeyManager(),
                                        new SimplePSKKeyManager() },
                     new TrustManager[] { new SimpleX509TrustManager() },
                     SecureRandom.getInstance("Fortuna"));
        Security.setProperty("jessie.client.psk.identity", "MAUVE");
      }
    catch (Exception e)
      {
        harness.fail("SSLContext.getInstance");
        harness.debug(e);
        return false;
      }
    return true;
  }


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
            // Only test PSK suites.
            if (suites[j].indexOf("PSK") < 0)
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
