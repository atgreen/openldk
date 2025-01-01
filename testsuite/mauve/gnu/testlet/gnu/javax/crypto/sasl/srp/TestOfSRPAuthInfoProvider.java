/* TestOfSRPAuthInfoProvider.java -- 
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

package gnu.testlet.gnu.javax.crypto.sasl.srp;

import gnu.java.security.Registry;
import gnu.java.security.util.Util;
import gnu.javax.crypto.sasl.IAuthInfoProvider;
import gnu.javax.crypto.sasl.srp.SRPAuthInfoProvider;
import gnu.javax.crypto.sasl.srp.SRPRegistry;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.HashMap;
import java.util.Map;
import java.util.Random;
import java.io.File;

import javax.security.sasl.AuthenticationException;

/**
 * Regression tests for SRP authentication provider (password file based).
 */
public class TestOfSRPAuthInfoProvider implements Testlet
{
  private Random prng = new Random();

  public void test(TestHarness harness)
  {
    harness.checkPoint("TestOfSRPAuthInfoProvider");

    String pFile = "./__TestOfSRPAuthInfoProvider";
    Map context = new HashMap();
    context.put(SRPRegistry.PASSWORD_FILE, pFile);

    IAuthInfoProvider authenticator = new SRPAuthInfoProvider();
    try
      {
        authenticator.activate(context);
        harness.check(true, "activate()");
      }
    catch (AuthenticationException x)
      {
        harness.debug(x);
        harness.fail("activate()");
      }

    byte[] salt = new byte[10];

    Map user1 = new HashMap();
    user1.put(Registry.SASL_USERNAME, "user1");
    user1.put(Registry.SASL_PASSWORD, "password1");
    user1.put(SRPRegistry.CONFIG_NDX_FIELD, "1");
    prng.nextBytes(salt);
    user1.put(SRPRegistry.SALT_FIELD, Util.toBase64(salt));
    user1.put(SRPRegistry.MD_NAME_FIELD, Registry.MD5_HASH);

    Map user2 = new HashMap();
    user2.put(Registry.SASL_USERNAME, "user2");
    user2.put(Registry.SASL_PASSWORD, "password2");
    user2.put(SRPRegistry.CONFIG_NDX_FIELD, "2");
    prng.nextBytes(salt);
    user2.put(SRPRegistry.SALT_FIELD, Util.toBase64(salt));
    user2.put(SRPRegistry.MD_NAME_FIELD, Registry.SHA1_HASH);

    Map user3 = new HashMap();
    user3.put(Registry.SASL_USERNAME, "user3");
    user3.put(Registry.SASL_PASSWORD, "password3");
    user3.put(SRPRegistry.CONFIG_NDX_FIELD, "3");
    prng.nextBytes(salt);
    user3.put(SRPRegistry.SALT_FIELD, Util.toBase64(salt));
    user3.put(SRPRegistry.MD_NAME_FIELD, Registry.MD5_HASH);

    Map user4 = new HashMap();
    user4.put(Registry.SASL_USERNAME, "user4");
    user4.put(Registry.SASL_PASSWORD, "password4");
    user4.put(SRPRegistry.CONFIG_NDX_FIELD, "4");
    prng.nextBytes(salt);
    user4.put(SRPRegistry.SALT_FIELD, Util.toBase64(salt));
    user4.put(SRPRegistry.MD_NAME_FIELD, Registry.SHA_HASH);

    Map credentials;
    try
      {
        credentials = authenticator.lookup(user1);
        harness.fail("lookup(user1)");
      }
    catch (AuthenticationException x)
      {
        harness.check(true, "lookup(user1)");
      }

    try
      {
        credentials = authenticator.lookup(user2);
        harness.fail("lookup(user2)");
      }
    catch (AuthenticationException x)
      {
        harness.check(true, "lookup(user2)");
      }

    try
      {
        credentials = authenticator.lookup(user3);
        harness.fail("lookup(user3)");
      }
    catch (AuthenticationException x)
      {
        harness.check(true, "lookup(user3)");
      }

    try
      {
        credentials = authenticator.lookup(user4);
        harness.fail("lookup(user4)");
      }
    catch (AuthenticationException x)
      {
        harness.check(true, "lookup(user4)");
      }

    // ----------------------------------------------------------------------

    try
      {
        authenticator.update(user1);
        harness.check(true, "update(user1)");
      }
    catch (AuthenticationException x)
      {
        harness.debug(x);
        harness.fail("update(user1)");
      }

    try
      {
        authenticator.update(user2);
        harness.check(true, "update(user2)");
      }
    catch (AuthenticationException x)
      {
        harness.debug(x);
        harness.fail("update(user2)");
      }

    try
      {
        authenticator.update(user3);
        harness.check(true, "update(user3)");
      }
    catch (AuthenticationException x)
      {
        harness.debug(x);
        harness.fail("update(user3)");
      }

    // ----------------------------------------------------------------------

    String saltString;
    try
      {
        credentials = authenticator.lookup(user1);
        saltString = (String) credentials.get(SRPRegistry.SALT_FIELD);
        harness.check(saltString.equals(user1.get(SRPRegistry.SALT_FIELD)),
                      "user1 OK");
      }
    catch (AuthenticationException x)
      {
        harness.fail("update(user1)");
      }

    try
      {
        credentials = authenticator.lookup(user2);
        saltString = (String) credentials.get(SRPRegistry.SALT_FIELD);
        harness.check(saltString.equals(user2.get(SRPRegistry.SALT_FIELD)),
                      "user2 OK");
      }
    catch (AuthenticationException x)
      {
        harness.fail("update(user2)");
      }

    try
      {
        credentials = authenticator.lookup(user3);
        saltString = (String) credentials.get(SRPRegistry.SALT_FIELD);
        harness.check(saltString.equals(user3.get(SRPRegistry.SALT_FIELD)),
                      "user3 OK");
      }
    catch (AuthenticationException x)
      {
        harness.fail("update(user3)");
      }

    // ----------------------------------------------------------------------

    try
      {
        authenticator.update(user4);
        harness.check(true, "update(user4)");
      }
    catch (AuthenticationException x)
      {
        harness.debug(x);
        harness.fail("update(user4)");
      }

    // ----------------------------------------------------------------------

    try
      {
        authenticator.passivate();
        harness.check(true, "passivate()");
      }
    catch (AuthenticationException x)
      {
        harness.debug(x);
        harness.fail("passivate()");
      }

    try
      {
        authenticator.activate(context);
        harness.check(true, "activate()");
      }
    catch (AuthenticationException x)
      {
        harness.debug(x);
        harness.fail("activate()");
      }

    try
      {
        credentials = authenticator.lookup(user4);
        saltString = (String) credentials.get(SRPRegistry.SALT_FIELD);
        harness.check(saltString.equals(user4.get(SRPRegistry.SALT_FIELD)));
      }
    catch (AuthenticationException x)
      {
        harness.fail("lookup(user4)");
      }

    try
      {
        new File(pFile).delete(); // remove test file
      }
    catch (Exception ignored)
      {
      }
  }
}