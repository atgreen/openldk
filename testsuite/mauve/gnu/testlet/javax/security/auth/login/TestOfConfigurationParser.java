/* TestOfConfigurationParser.java -- GnuConfiguration parser conformance tests
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

package gnu.testlet.javax.security.auth.login;

import java.io.Reader;
import java.io.StringReader;
import java.util.Map;

import gnu.javax.security.auth.login.ConfigFileParser;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Simple tests to check conformane with published documentation.
 */
public class TestOfConfigurationParser implements Testlet
{
  /** Example from Configuration.java. */ 
  private static final String TC1 = "\n" +
  " Login {\n" +
  "   com.sun.security.auth.module.UnixLoginModule required;\n" +
  "   com.sun.security.auth.module.Krb5LoginModule optional\n" +
  "                   useTicketCache=\"true\"\n" +
  "                   ticketCache=\"${user.home}${/}tickets\";\n" +
  " } ;\n";

  /** Example with UTf-8 characters. */
  private static final String TC2 = "\n" +
  " Pilsçtâs {\n" +
  "   gnu.i18n.security.auth.module.SaslLoginModule required;\n" +
  "   gnu.i18n.security.auth.module.LdapLoginModule optional\n" +
  "                   use=\"Wörterbuch\"\n" +
  "                   dir=\"${user.home}${/}Hervé\";\n" +
  " } ;\n";

  /** Example from Configuration.java with compact syntax. */ 
  private static final String TC3 =
  "Login{" +
      "com.sun.security.auth.module.UnixLoginModule required;" +
      "com.sun.security.auth.module.Krb5LoginModule optional " +
          "useTicketCache=\"true\" " +
          "ticketCache=\"${user.home}${/}tickets\";" +
  "};" +
  " Pilsçtâs{" +
      "gnu.i18n.security.auth.module.SaslLoginModule required;" +
      "gnu.i18n.security.auth.module.LdapLoginModule optional " +
          "use=\"Wörterbuch\" " +
          "dir=\"${user.home}${/}Hervé\";" +
  "};";

  /** Example with different types of comments. */ 
  private static final String TC4 =
  "Pilsçtâs /* the application name */{\n" +
  "// a java-style comment \n" +
  "gnu.pkg.module.LM$Sasl requisite;\n" +
  "# bash-style comment\n" +
  "gnu.pkg.module.LM$Ldap /* another in-line comment */ sufficient\n" +
  "use=\"Wörterbuch\"\n" +
  "dir=\"${user.home}${/}Hervé\";\n" +
  " } ;\n";

  /** Sample login config file from JAAS Tutorial jdk 1.4. */
  private static final String TC5 = "" +
  "/** Login Configuration for the JAAS Sample Application **/" +
  "Sample {" +
  "   sample.module.SampleLoginModule required debug=true;" +
  "};";

  private TestHarness harness;
  private ConfigFileParser cp;

  public void test(TestHarness harness)
  {
    this.harness = harness;
    setUp();
    parseConfig(TC1, "MUST parse Configuration javadoc example");
    parseConfig(TC2, "MUST parse entities in UTF-8 encoding");
    parseConfig(TC3, "MUST parse compact syntax");
    parseConfig(TC4, "MUST ignore comments");
    parseConfig(TC5, "MUST parse JAAS tutorial example");
    teardown();
  }

  private void setUp()
  {
    cp = new ConfigFileParser();
  }

  private void parseConfig(String s, String m)
  {
    harness.checkPoint("parseConfig");
    try
      {
        Reader sr = new StringReader(s);
        cp.parse(sr);
        Map map = cp.getLoginModulesMap();
        harness.check(map.size() > 0, m);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail(m);
      }
  }

  private void teardown()
  {
    cp = null;
  }
}
