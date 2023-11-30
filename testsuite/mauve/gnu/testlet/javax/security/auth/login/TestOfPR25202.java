/* TestOfGnuConfiguration.java -- Regression test for PR25202
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
// Uses: DBLoginModule

package gnu.testlet.javax.security.auth.login;

import java.io.File;
import java.io.FileWriter;

import javax.security.auth.callback.Callback;
import javax.security.auth.callback.CallbackHandler;
import javax.security.auth.login.LoginContext;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Simple tests to check compliance with published documentation.
 */
public class TestOfPR25202 implements Testlet
{
  private static final String CONFIG = "" +
      "DBLogin {\n" +
      "        gnu.testlet.javax.security.auth.login.DBLoginModule required;\n" +
      "};";

  private TestHarness harness;

  public void test(TestHarness harness)
  {
    this.harness = harness;
    setUp();
    pr25202();
    teardown();
  }

  private void pr25202()
  {
    harness.checkPoint("pr25202");
    try
      {
        LoginContext lc = new LoginContext("DBLogin",
                                           new DefaultLoginHandler("", "", ""));
        lc.login();
        harness.check(true, "MUST be able to login");
        lc.logout();
        harness.check(true, "MUST be able to logout");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("pr25202");
      }
  }

  private void setUp()
  {
    harness.checkPoint("setUp");
    try
      {
        File cf = File.createTempFile("auth", ".login");
        cf.deleteOnExit();
        FileWriter fw = new FileWriter(cf);
        fw.write(CONFIG);
        fw.close();
        String cfPath = cf.getCanonicalPath();
        System.setProperty("java.security.auth.login.config", cfPath);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("setUp");
      }
  }

  private void teardown()
  {
  }

  // Inner class(es)
  // --------------------------------------------------------------------------

  public class DefaultLoginHandler implements CallbackHandler
  {
    public DefaultLoginHandler(String username, String password, String domain)
    {
      super();
    }

    public void handle(Callback[] callbacks)
    {
    }
  }
}
