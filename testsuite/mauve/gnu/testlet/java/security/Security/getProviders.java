// $Id: getProviders.java,v 1.2 2006/01/03 09:24:39 raif Exp $
//
// Copyright (C) 2003 Free Software Foundation, Inc.
//
// This file is part of Mauve.
//
// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

// Tags: JDK1.3

package gnu.testlet.java.security.Security;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import java.security.Security;
import java.security.Provider;
import java.security.InvalidParameterException;

/**
 * Test of <code>getProviders(String)</code> and <code>getProviders(Map)
 * methods in {@link java.security.Security}.
 *
 * @version $Revision: 1.2 $
 * @see java.security.Security#getProviders(String)
 * @see java.security.Security#getProviders(java.util.Map)
 */
public class getProviders implements Testlet
{
  Provider tom = new Tom();
  Provider dick = new Dick();
  Provider harry = new Harry();

  public void test (TestHarness harness)
  {
    harness.checkPoint("getProviders(String)");

    testNoProviders(harness);
    test1Provider(harness);
    test2Providers(harness);
    test3Providers(harness);
  }

  private void testNoProviders(TestHarness harness)
  {
    String filter = "NoService.NoAlgorithm";
    // try with dummy filter and no providers installed
    try
      {
        harness.check(Security.getProviders(filter), null);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("testNoProviders.1: "+String.valueOf(x));
      }
  }

  private void test1Provider(TestHarness harness)
  {
    Security.addProvider(tom);

    String signature = "Security.getProvider(\"tom\")";
    try
      {
        Provider sameProvider = Security.getProvider("  Tom  ");
        harness.check(sameProvider != null, signature);
      }
    catch (Throwable x)
      {
        harness.fail(signature);
        harness.debug(x);
      }

    String filter = "NoService.NoAlgorithm";
    // try dummy filter with one known provider
    try
      {
        harness.check(Security.getProviders(filter), null);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("test1Provider.2: "+String.valueOf(x));
      }

    Provider[] providers;
    // try real filter with one known provider
    filter = "CoffeeMaker.Foo";
    try
      {
        providers = Security.getProviders(filter);
        if (providers == null || providers.length != 1)
          harness.fail("Tom : getProviders(\""+filter+"\")");
        else
          harness.check(providers[0], tom);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("test1Provider.3: "+String.valueOf(x));
      }

    // try real filter (different case) with one known provider
    filter = "CoffeeMaker.FOO";
    try
      {
        providers = Security.getProviders(filter);
        if (providers == null || providers.length != 1)
          harness.fail("Tom : getProviders(\""+filter+"\")");
        else
          harness.check(providers[0], tom);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("test1Provider.4: "+String.valueOf(x));
      }

    // try incorrect (syntax) filter with one known provider
    filter = "CoffeeMakerFoo";
    try
      {
        providers = Security.getProviders(filter);
        harness.fail("Tom : getProviders(\""+filter+"\")");
      }
    catch (InvalidParameterException x) // expected
      {
        harness.check(true);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("test1Provider.5: "+String.valueOf(x));
      }

    // try filter alias (1 indirection) with one known provider
    filter = "CoffeeMaker.Bar";
    try
      {
        providers = Security.getProviders(filter);
        if (providers == null || providers.length != 1)
          harness.fail("Tom : getProviders(\""+filter+"\")");
        else
          harness.check(providers[0], tom);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("test1Provider.6: "+String.valueOf(x));
      }

    // try filter alias (2 indirections) with one known provider
    filter = "CoffeeMaker.WHAT";
    try
      {
        providers = Security.getProviders(filter);
        if (providers == null || providers.length != 1)
          harness.fail("Tom : getProviders(\""+filter+"\")");
        else
          harness.check(providers[0], tom);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("test1Provider.7: "+String.valueOf(x));
      }

    // try real filter (incl. attr only) with one known provider
    filter = "CoffeeMaker.FOO ImplementedIn:vapourware";
    try
      {
        providers = Security.getProviders(filter);
        if (providers == null || providers.length != 1)
          harness.fail("Tom : getProviders(\""+filter+"\")");
        else
          harness.check(providers[0], tom);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("test1Provider.8: "+String.valueOf(x));
      }

    // try real filter (incl. attr+val) with one known provider
    filter = "CoffeeMaker.FOO minCapacity:150";
    try
      {
        providers = Security.getProviders(filter);
        if (providers == null || providers.length != 1)
          harness.fail("Tom : getProviders(\""+filter+"\")");
        else
          harness.check(providers[0], tom);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("test1Provider.9: "+String.valueOf(x));
      }
  }

  private void test2Providers(TestHarness harness)
  {
    Security.addProvider(dick);

    Provider[] providers;
    String filter = "NoService.NoAlgorithm";

    // try real filter (incl. attr only) with two known providers
    filter = "CoffeeMaker.FOO ImplementedIn:vapourware";
    try
      {
        providers = Security.getProviders(filter);
        if (providers == null || providers.length != 2
            || (providers[0] != tom && providers[1] != dick))
          harness.fail("Tom, Dick : getProviders(\""+filter+"\")");
        else
          harness.check(true);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("test2Providers.1: "+String.valueOf(x));
      }

    // try real filter (incl. attr+val) with two known providers
    filter = "CoffeeMaker.FOO minCapacity:150";
    try
      {
        providers = Security.getProviders(filter);
        if (providers == null || providers.length != 2
            || (providers[0] != tom && providers[1] != dick))
          harness.fail("Tom, Dick : getProviders(\""+filter+"\")");
        else
          harness.check(true);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("test2Providers.2: "+String.valueOf(x));
      }
  }

  private void test3Providers(TestHarness harness)
  {
    Security.addProvider(harry);

    Provider[] p;
    String filter = "NoService.NoAlgorithm";

    // try real filter (incl. attr only) with three known providers
    filter = "CoffeeMaker.FOO ImplementedIn:vapourware";
    try
      {
        p = Security.getProviders(filter);
        if (p == null || p.length != 3 ||
            (p[0] != tom && p[1] != dick && p[2] != harry))
          harness.fail("Tom, Dick, Harry : getProviders(\""+filter+"\")");
        else
          harness.check(true);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("test3Providers.1: "+String.valueOf(x));
      }

    // try real filter (incl. attr+val) with three known providers
    filter = "CoffeeMaker.FOO minCapacity:150";
    try
      {
        p = Security.getProviders(filter);
        if (p == null || p.length != 3 ||
            (p[0] != tom && p[1] != dick && p[2] != harry))
          harness.fail("Tom, Dick, Harry : getProviders(\""+filter+"\")");
        else
          harness.check(true);
      }
    catch (Exception x)
      {
        harness.debug(x);
        harness.fail("test3Providers.2: "+String.valueOf(x));
      }
  }

  // Inner class(es)
  // ==========================================================================

  class Tom extends Provider
  {
    Tom()
    {
      super("Tom", 1.0, "");

      put("CoffeeMaker.Foo", "acme.crypto.FooSpi");
      put("CoffeeMaker.Foo ImplementedIn", "Vapourware");
      put("CoffeeMaker.Foo MinCapacity", "100");
      put("Alg.Alias.CoffeeMaker.bar", "fOO");
      put("Alg.Alias.CoffeeMaker.what", "bar");
    }
  }

  class Dick extends Provider
  {
    Dick()
    {
      super("Dick", 2.0, "");

      put("CoffeeMaker.Foo", "acme.crypto.FooSpi");
      put("CoffeeMaker.Foo ImplementedIn", "Vapourware");
      put("CoffeeMaker.Foo MinCapacity", "120");
      put("Alg.Alias.CoffeeMaker.bar", "fOO");
      put("Alg.Alias.CoffeeMaker.what", "bar");
    }
  }

  class Harry extends Provider
  {
    Harry()
    {
      super("Harry", 3.0, "");

      put("CoffeeMaker.Foo", "acme.crypto.FooSpi");
      put("CoffeeMaker.Foo ImplementedIn", "Vapourware");
      put("CoffeeMaker.Foo MinCapacity", "140");
      put("Alg.Alias.CoffeeMaker.bar", "fOO");
      put("Alg.Alias.CoffeeMaker.what", "bar");
    }
  }
}
