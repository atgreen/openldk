/* TestNoCiphersuites.java -- test no common ciphersuites.
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

/**
 * Test a connection between two SSLEngines with no cipher suites in common
 * (which should fail).
 *
 * @author Casey Marshall (csm@gnu.org)
 */
public class TestNoCiphersuites
  extends AbstractEngineTest
{

  /* (non-Javadoc)
   * @see gnu.testlet.javax.net.ssl.SSLEngine.AbstractEngineTest#implTest(gnu.testlet.TestHarness)
   */
  protected void implTest(TestHarness harness)
  {
    setupEngines(harness);
    
    clientEngine.setEnabledCipherSuites(new String[] {
      "TLS_DHE_DSS_WITH_AES_256_CBC_SHA",
      "TLS_DHE_RSA_WITH_AES_256_CBC_SHA",
      "TLS_DH_DSS_WITH_AES_256_CBC_SHA",
      "TLS_DH_RSA_WITH_AES_256_CBC_SHA",
      "TLS_RSA_WITH_AES_256_CBC_SHA",
      "TLS_DHE_DSS_WITH_AES_128_CBC_SHA",
      "TLS_DHE_RSA_WITH_AES_128_CBC_SHA",
      "TLS_DH_DSS_WITH_AES_128_CBC_SHA",
      "TLS_DH_RSA_WITH_AES_128_CBC_SHA",
      "TLS_RSA_WITH_AES_128_CBC_SHA",
      "TLS_DHE_DSS_WITH_3DES_EDE_CBC_SHA",
      "TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA",
      "TLS_DH_DSS_WITH_3DES_EDE_CBC_SHA",
      "TLS_DH_RSA_WITH_3DES_EDE_CBC_SHA",
      "TLS_RSA_WITH_3DES_EDE_CBC_SHA"
    });
    
    serverEngine.setEnabledCipherSuites(new String[] {
      "TLS_RSA_WITH_RC4_128_MD5",
      "TLS_RSA_WITH_RC4_128_SHA",
      "TLS_DHE_DSS_WITH_DES_CBC_SHA",
      "TLS_DHE_RSA_WITH_DES_CBC_SHA",
      "TLS_DH_DSS_WITH_DES_CBC_SHA",
      "TLS_DH_RSA_WITH_DES_CBC_SHA",
      "TLS_RSA_WITH_DES_CBC_SHA",
      "TLS_DH_DSS_EXPORT_WITH_DES40_CBC_SHA",
      "TLS_DH_RSA_EXPORT_WITH_DES40_CBC_SHA",
      "TLS_RSA_EXPORT_WITH_DES40_CBC_SHA",
      "TLS_RSA_EXPORT_WITH_RC4_40_MD5",
      "TLS_DHE_DSS_EXPORT_WITH_DES40_CBC_SHA",
      "TLS_DHE_RSA_EXPORT_WITH_DES40_CBC_SHA",
      "TLS_RSA_WITH_NULL_MD5",
      "TLS_RSA_WITH_NULL_SHA"
    });
    
    harness.checkPoint("SSLEngine/no-ciphersuites");
    
    try
      {
        runHandshake();
        harness.fail("SSLEngine/no-ciphersuites");
      }
    catch (Exception x)
      {
        harness.check(true);
      }
  }
}
