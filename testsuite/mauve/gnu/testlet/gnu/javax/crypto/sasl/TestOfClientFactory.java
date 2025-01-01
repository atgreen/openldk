/* TestOfClientFactory.java -- 
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

package gnu.testlet.gnu.javax.crypto.sasl;

import gnu.java.security.Registry;
import gnu.javax.crypto.sasl.ClientFactory;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.HashMap;

import javax.security.sasl.Sasl;

/**
 * Regression tests for SASL Client factories.
 */
public class TestOfClientFactory implements Testlet
{
  private static boolean includes(String[] sa, String n)
  {
    for (int i = 0; i < sa.length; i++)
      if (n.equals(sa[i]))
        return true;
    return false;
  }

  public void test(TestHarness harness)
  {
    harness.checkPoint("TestOfClientFactory:null");

    ClientFactory factory = new ClientFactory();
    String[] mechanisms = factory.getMechanismNames(null);

    // should see all mechanisms
    harness.check(includes(mechanisms, Registry.SASL_SRP_MECHANISM),
                  Registry.SASL_SRP_MECHANISM);
    harness.check(includes(mechanisms, Registry.SASL_CRAM_MD5_MECHANISM),
                  Registry.SASL_CRAM_MD5_MECHANISM);
    harness.check(includes(mechanisms, Registry.SASL_PLAIN_MECHANISM),
                  Registry.SASL_PLAIN_MECHANISM);
    harness.check(includes(mechanisms, Registry.SASL_ANONYMOUS_MECHANISM),
                  Registry.SASL_ANONYMOUS_MECHANISM);

    harness.checkPoint("TestOfClientFactory:" + Sasl.POLICY_NOPLAINTEXT);
    HashMap p = new HashMap();
    p.put(Sasl.POLICY_NOPLAINTEXT, "true");
    mechanisms = factory.getMechanismNames(p);

    // should see all mechanisms except PLAIN
    harness.check(includes(mechanisms, Registry.SASL_SRP_MECHANISM),
                  Registry.SASL_SRP_MECHANISM);
    harness.check(includes(mechanisms, Registry.SASL_CRAM_MD5_MECHANISM),
                  Registry.SASL_CRAM_MD5_MECHANISM);
    harness.check(!includes(mechanisms, Registry.SASL_PLAIN_MECHANISM),
                  Registry.SASL_PLAIN_MECHANISM);
    harness.check(includes(mechanisms, Registry.SASL_ANONYMOUS_MECHANISM),
                  Registry.SASL_ANONYMOUS_MECHANISM);

    harness.checkPoint("TestOfClientFactory:" + Sasl.POLICY_NOACTIVE);
    p.clear();
    p.put(Sasl.POLICY_NOACTIVE, "true");
    mechanisms = factory.getMechanismNames(p);

    // should see all mechanisms except PLAIN & CRAM-MD5
    harness.check(includes(mechanisms, Registry.SASL_SRP_MECHANISM),
                  Registry.SASL_SRP_MECHANISM);
    harness.check(!includes(mechanisms, Registry.SASL_CRAM_MD5_MECHANISM),
                  Registry.SASL_CRAM_MD5_MECHANISM);
    harness.check(!includes(mechanisms, Registry.SASL_PLAIN_MECHANISM),
                  Registry.SASL_PLAIN_MECHANISM);
    harness.check(includes(mechanisms, Registry.SASL_ANONYMOUS_MECHANISM),
                  Registry.SASL_ANONYMOUS_MECHANISM);

    harness.checkPoint("TestOfClientFactory:" + Sasl.POLICY_NODICTIONARY);
    p.clear();
    p.put(Sasl.POLICY_NODICTIONARY, "true");
    mechanisms = factory.getMechanismNames(p);

    // should see all mechanisms except PLAIN & CRAM-MD5
    harness.check(includes(mechanisms, Registry.SASL_SRP_MECHANISM),
                  Registry.SASL_SRP_MECHANISM);
    harness.check(!includes(mechanisms, Registry.SASL_CRAM_MD5_MECHANISM),
                  Registry.SASL_CRAM_MD5_MECHANISM);
    harness.check(!includes(mechanisms, Registry.SASL_PLAIN_MECHANISM),
                  Registry.SASL_PLAIN_MECHANISM);
    harness.check(includes(mechanisms, Registry.SASL_ANONYMOUS_MECHANISM),
                  Registry.SASL_ANONYMOUS_MECHANISM);

    harness.checkPoint("TestOfClientFactory:" + Sasl.POLICY_NOANONYMOUS);
    p.clear();
    p.put(Sasl.POLICY_NOANONYMOUS, "true");
    mechanisms = factory.getMechanismNames(p);

    // should see all mechanisms except ANONYMOUS
    harness.check(includes(mechanisms, Registry.SASL_SRP_MECHANISM),
                  Registry.SASL_SRP_MECHANISM);
    harness.check(includes(mechanisms, Registry.SASL_CRAM_MD5_MECHANISM),
                  Registry.SASL_CRAM_MD5_MECHANISM);
    harness.check(includes(mechanisms, Registry.SASL_PLAIN_MECHANISM),
                  Registry.SASL_PLAIN_MECHANISM);
    harness.check(!includes(mechanisms, Registry.SASL_ANONYMOUS_MECHANISM),
                  Registry.SASL_ANONYMOUS_MECHANISM);

    harness.checkPoint("TestOfClientFactory:" + Sasl.POLICY_FORWARD_SECRECY);
    p.clear();
    p.put(Sasl.POLICY_FORWARD_SECRECY, "true");
    mechanisms = factory.getMechanismNames(p);

    // should see all mechanisms except ANONYMOUS,PLAIN & CRAM-MD5
    harness.check(includes(mechanisms, Registry.SASL_SRP_MECHANISM),
                  Registry.SASL_SRP_MECHANISM);
    harness.check(!includes(mechanisms, Registry.SASL_CRAM_MD5_MECHANISM),
                  Registry.SASL_CRAM_MD5_MECHANISM);
    harness.check(!includes(mechanisms, Registry.SASL_PLAIN_MECHANISM),
                  Registry.SASL_PLAIN_MECHANISM);
    harness.check(!includes(mechanisms, Registry.SASL_ANONYMOUS_MECHANISM),
                  Registry.SASL_ANONYMOUS_MECHANISM);

    harness.checkPoint("TestOfClientFactory:" + Sasl.POLICY_PASS_CREDENTIALS);
    p.clear();
    p.put(Sasl.POLICY_PASS_CREDENTIALS, "true");
    mechanisms = factory.getMechanismNames(p);

    // should see none
    harness.check(!includes(mechanisms, Registry.SASL_SRP_MECHANISM),
                  Registry.SASL_SRP_MECHANISM);
    harness.check(!includes(mechanisms, Registry.SASL_CRAM_MD5_MECHANISM),
                  Registry.SASL_CRAM_MD5_MECHANISM);
    harness.check(!includes(mechanisms, Registry.SASL_PLAIN_MECHANISM),
                  Registry.SASL_PLAIN_MECHANISM);
    harness.check(!includes(mechanisms, Registry.SASL_ANONYMOUS_MECHANISM),
                  Registry.SASL_ANONYMOUS_MECHANISM);
  }
}