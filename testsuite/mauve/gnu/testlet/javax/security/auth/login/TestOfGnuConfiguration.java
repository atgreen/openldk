/* TestOfGnuConfiguration.java -- Conformance tests for GnuConfiguration
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
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.channels.FileChannel;
import java.security.Security;

import javax.security.auth.callback.Callback;
import javax.security.auth.callback.CallbackHandler;
import javax.security.auth.login.Configuration;
import javax.security.auth.login.LoginContext;
import javax.security.auth.login.LoginException;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Simple tests for conformance of code to published documentation.
 */
public class TestOfGnuConfiguration implements Testlet
{
  private static final String CONFIG = ""
     + "DBLogin {\n"
     + "        gnu.testlet.javax.security.auth.login.DBLoginModule required;\n"
     + "};";

  private TestHarness harness;
  private String cfPath;

  public void test(TestHarness harness)
  {
    this.harness = harness;
    setUp();
    parseFromSecurityProperty();
    parseFromSystemProperty();
    parseFromUserHome();
    nullConfiguration();
    teardown();
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

      cfPath = cf.getCanonicalPath();
    }
    catch (Exception x)
    {
      harness.debug(x);
      harness.fail("setUp");
    }
  }

  private void parseFromSecurityProperty()
  {
    harness.checkPoint("parseFromSecurityProperty");
    try
      {
        Security.setProperty("java.security.auth.login.config.url.1", cfPath);
//        Configuration.getConfiguration().refresh();

        LoginContext lc =
            new LoginContext("DBLogin", new DefaultLoginHandler("", "", ""));
        lc.login();
        harness.check(true, "MUST be able to login");
        lc.logout();
        harness.check(true, "MUST be able to logout");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("parseFromSecurityProperty");
      }
  }

  private void parseFromSystemProperty()
  {
    harness.checkPoint("parseFromSystemProperty");
    try
      {
        Security.setProperty("java.security.auth.login.config.url.1", "");
        System.setProperty("java.security.auth.login.config", cfPath);
        Configuration.getConfiguration().refresh();

        LoginContext lc =
            new LoginContext("DBLogin", new DefaultLoginHandler("", "", ""));
        lc.login();
        harness.check(true, "MUST be able to login");
        lc.logout();
        harness.check(true, "MUST be able to logout");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("parseFromSystemProperty");
      }
  }

  private void parseFromUserHome()
  {
    harness.checkPoint("parseFromUserHome");
    File myConfig = null;
    try
      {
        Security.setProperty("java.security.auth.login.config.url.1", "");
        System.setProperty("java.security.auth.login.config", "");
        myConfig =
            new File(System.getProperty("user.home"), ".java.login.config");
        myConfig.deleteOnExit();
        copy(new File(cfPath), myConfig);
        Configuration.getConfiguration().refresh();

        LoginContext lc =
            new LoginContext("DBLogin", new DefaultLoginHandler("", "", ""));
        lc.login();
        harness.check(true, "MUST be able to login");
        lc.logout();
        harness.check(true, "MUST be able to logout");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("parseFromUserHome");
      }
    finally
      {
        if (myConfig != null)
          myConfig.delete();
      }
  }

  private void nullConfiguration()
  {
    harness.checkPoint("nullConfiguration");
    try
      {
        Security.setProperty("java.security.auth.login.config.url.1", "");
        System.setProperty("java.security.auth.login.config", "");
        Configuration.getConfiguration().refresh();

        try
          {
            new LoginContext("DBLogin", new DefaultLoginHandler("", "", ""));
            harness.fail("MUST NOT be able to create context");
          }
        catch (LoginException x)
          {
            harness.check(true, "MUST NOT be able to create context");
          }
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("nullConfiguration");
      }
  }

  private void teardown()
  {
  }

  private void copy(File src, File dst) throws IOException
  {
    if (!dst.exists())
      dst.createNewFile();

    FileChannel source = null;
    FileChannel destination = null;
    try
      {
        source = new FileInputStream(src).getChannel();
        destination = new FileOutputStream(dst).getChannel();
        destination.transferFrom(source, 0, source.size());
      }
    finally
      {
        if (source != null)
          source.close();

        if (destination != null)
          destination.close();
      }
  }

  // Inner class(es)
  // --------------------------------------------------------------------------

  class DefaultLoginHandler implements CallbackHandler
  {
    public DefaultLoginHandler(String u, String p, String d)
    {
      super();
    }

    public void handle(Callback[] callbacks)
    {
    }
  }
}
