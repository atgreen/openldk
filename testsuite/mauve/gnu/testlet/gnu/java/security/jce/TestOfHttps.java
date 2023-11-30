/* TestOfHttps.java
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

package gnu.testlet.gnu.java.security.jce;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.security.Security;

import gnu.javax.crypto.jce.GnuCrypto;
import gnu.javax.net.ssl.provider.Jessie;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Regression test for HTTPS functionality with newly integrated GNU Crypto in
 * Classpath.
 */
public class TestOfHttps
    implements Testlet
{
  // default 0-arguments constructor

  public void test(TestHarness harness)
  {
    String msg = "MUST be able to verify X.509 certificates";
    try
    {
      Security.addProvider(new Jessie());
      Security.addProvider(new GnuCrypto());
      URL u = new URL("https://www.paypal.com/");
      InputStream in = u.openStream();
      BufferedReader br = new BufferedReader(new InputStreamReader(in));
      String line;
      while ((line = br.readLine()) != null)
        ; // do nothing

      harness.check(true, msg);
    }
    catch (Exception x)
    {
      harness.debug(x);
      harness.fail(msg);
    }
  }
}
