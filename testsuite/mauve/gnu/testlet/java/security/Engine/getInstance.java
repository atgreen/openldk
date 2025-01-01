/* getInstance.java -- Ensure names with extra spaces are recognized
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

// Tags: JDK1.4
// Uses: ../MessageDigest/MauveDigest

package gnu.testlet.java.security.Engine;

import gnu.java.security.Engine;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.security.NoSuchAlgorithmException;
import java.security.Provider;
import java.security.Security;

public class getInstance extends Provider implements Testlet
{
  
  private Provider provider;
    
  public getInstance()
  {
    super("FakeProvider", 1.0, "A Fake Provider Used Within the Mauve Test Suite");

    put("MessageDigest.foo",
        "gnu.testlet.java.security.MessageDigest.MauveDigest");
    put("Alg.Alias.MessageDigest.bar", "foo");
  }

  
  // Test case for the behaviour of
  // Engine.getInstance (service, algorithm, provider).
  // White space should be ignored.
  // The algorithm names should be case insensitive.
  public void test (TestHarness harness){
    setUp (harness);
    testWhiteSpace(harness);
    testAlgorithmCase (harness);
    testNameRedundancy(harness);
  }

  private void setUp (TestHarness harness){
    provider = this;
    Security.addProvider(provider);
  }
  
  // Tests the behaviour of 
  // Engine.getInstance (service, algorithm, provider).
  // The algorithms and service names should ignore any white space.
  private void testWhiteSpace (TestHarness harness)
  {
    harness.checkPoint ("Engine");
    String signature;

    signature = "getInstance(\"MessageDigest\", \"foo\", provider)";
    try
      {
        harness.check(
            Engine.getInstance("MessageDigest", "foo", provider) != null,
            signature);
      }
    catch (Exception x)
      {
        harness.fail(signature);
        harness.debug(x);
      }

    signature = "getInstance(\"  MessageDigest  \", \"foo\", provider)";
    try
      {
        harness.check(
            Engine.getInstance("  MessageDigest  ", "foo", provider) != null,
            signature);
      }
    catch (Exception x)
      {
        harness.fail(signature);
        harness.debug(x);
      }

    signature = "getInstance(\"MessageDigest\", \"  foo  \", provider)";
    try
      {
        harness.check(
            Engine.getInstance("MessageDigest", "  foo  ", provider) != null,
            signature);
      }
    catch (Exception x)
      {
        harness.fail(signature);
        harness.debug(x);
      }

    signature = "getInstance(\"  MessageDigest  \", \"  foo  \", provider)";
    try
      {
        harness.check(
            Engine.getInstance("  MessageDigest  ", "  foo  ", provider) != null,
            signature);
      }
    catch (Exception x)
      {
        harness.fail(signature);
        harness.debug(x);
      }
  }

  // Tests the behaviour of 
  // Engine.getInstance (service, algorithm, provider).
  // The algorithm names should be case insensitive.
  private void testAlgorithmCase(TestHarness harness)
  {
    try
      {

        // test to make sure the engine can be found using all lowercase
        // characters.

        try
          {
            Engine.getInstance("MessageDigest", "foo", provider);
          }
        catch (NoSuchAlgorithmException e)
          {
            harness.fail("Could not find engine when using all lowercase characters");
            harness.debug(e);
          }

        // test to make sure the engine can be found using all uppercase
        // characters
        try
          {
            Engine.getInstance("MessageDigest", "FOO", provider);
          }
        catch (NoSuchAlgorithmException e)
          {
            harness.fail("Could not find engine when using all uppercase characters");
            harness.debug(e);
          }

        // test to make sure the engine can be found using a random case for the
        // characters
        try
          {
            Engine.getInstance("MessageDigest", "FoO", provider);
          }
        catch (NoSuchAlgorithmException e)
          {
            harness.fail("Could not find engine when using random case characters");
            harness.debug(e);
          }

        // test to make sure the engine can be found using the exact same case
        // specified in the Provider
        try
          {
            Engine.getInstance("MessageDigest", "foo", provider);
          }
        catch (NoSuchAlgorithmException e)
          {
            harness.fail("Could not find engine using exact case characters");
            harness.debug(e);
          }

        // test to make sure the engine can be found usinga all lowercase
        // characters using the alias
        try
          {
            Engine.getInstance("MessageDigest", "bar", provider);
          }
        catch (NoSuchAlgorithmException e)
          {
            harness.fail("Could not find engine using alias and all lowercase characters");
            harness.debug(e);
          }

        // test to make sure the engine can be found using all uppercase
        // characters using the alias
        try
          {
            Engine.getInstance("MessageDigest", "BAR", provider);
          }
        catch (NoSuchAlgorithmException e)
          {
            harness.fail("Could not find engine using alias and all uppercase characters");
            harness.debug(e);
          }

        // test to make sure the engine can be found using a random case for the
        // characters using the alias
        try
          {
            Engine.getInstance("MessageDigest", "bAr", provider);
          }
        catch (NoSuchAlgorithmException e)
          {
            harness.fail("Could not find engine using alias and random case characters");
            harness.debug(e);
          }

        // test to make sure the engine can be found using the exact same case
        // specified in the Provider using the alias
        try
          {
            Engine.getInstance("MessageDigest", "bar", provider);
          }
        catch (NoSuchAlgorithmException e)
          {
            harness.fail("Could not find engine using alias and exact case characters");
            harness.debug(e);
          }
      }

    catch (Exception e)
      {
        harness.debug(e);
        harness.fail(String.valueOf(e));
      }

  }

  /**
   * Tests that the Provider class is immune against adding/removing the same
   * algorithm with different case names.
   * 
   * @param harness the test harness.
   */
  private void testNameRedundancy(TestHarness harness)
  {
    harness.checkPoint("Engine.testNameRedundancy()");
    try
      {
        mustFindName(harness, "foo");
        mustFindName(harness, "FOO");

        // add a new spelling of 'foo'
        provider.put("MessageDigest.Foo",
                     "gnu.testlet.java.security.MessageDigest.MauveDigest");
        harness.verbose("*** Added 'Foo'");

        mustFindName(harness, "Foo");

        // now remove 'foo'.  all 'foo' spellings should not be found
        provider.remove("MessageDigest.foo");
        harness.verbose("*** Removed 'foo'");

        mustNotFindName(harness, "foo");
        mustNotFindName(harness, "FOO");
        mustNotFindName(harness, "Foo");

        // put 'foo' back
        put("MessageDigest.foo",
            "gnu.testlet.java.security.MessageDigest.MauveDigest");
        harness.verbose("*** Re-added 'foo'");
        // add a new spelling of 'bar'
        put("Alg.Alias.MessageDigest.Bar", "Foo");
        harness.verbose("*** Added alias 'Bar'");

        mustFindName(harness, "bar");
        mustFindName(harness, "BAR");
        mustFindName(harness, "Bar");

        // now remove 'bar'.  all 'bar' spellings should not be found
        provider.remove("Alg.Alias.MessageDigest.bar");
        harness.verbose("*** Removed alias 'bar'");

        mustNotFindName(harness, "bar");
        mustNotFindName(harness, "BAR");
        mustNotFindName(harness, "Bar");
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("Engine.testNameRedundancy(): " + x);
      }
  }

  private void mustFindName(TestHarness harness, String name)
  {
    String msg = "MUST find " + name;
    try
      {
        Object obj = Engine.getInstance("MessageDigest", name, provider);
        harness.check(obj != null, msg);
      }
    catch (Exception x)
      {
        harness.fail(msg);
        harness.debug(x);
      }
  }

  private void mustNotFindName(TestHarness harness, String name)
  {
    String msg = "MUST NOT find " + name;
    try
      {
        Object obj = Engine.getInstance("MessageDigest", name, provider);
        harness.check(obj == null, msg);
      }
    catch (NoSuchAlgorithmException x)
      {
        harness.check(true, msg);
      }
    catch (Exception x)
      {
        harness.fail(msg);
        harness.debug(x);
      }
  }
}
