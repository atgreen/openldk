// Copyright (C) 2010 Red Hat, Inc.
// Written by Andrew John Hughes <ahughes@redhat.com>

// This file is part of Mauve.

// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.

// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

// Tags: JDK1.4

package gnu.testlet.java.security.Policy;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.security.AllPermission;
import java.security.Policy;
import java.security.Permission;
import java.security.ProtectionDomain;

/**
 * Tests whether the default SecurityManager implementation
 * actually uses the policy set by setPolicy.
 */
public class setPolicy implements Testlet
{

  public void test(TestHarness harness)
  {
    TestSecurityManager sm = new TestSecurityManager();
    sm.install();
    sm.checkRead("/tmp");
    harness.check(sm.isCalled(), "Policy was checked");
  }

  private class TestSecurityManager extends SecurityManager
  {

    private boolean called = false;

    public void install()
    {
      Policy.setPolicy(new Policy()
        {
          public boolean implies(ProtectionDomain domain, Permission permission)
          {
            called = true;
            return true;
          }
        });
      System.setSecurityManager(this);
    }

    public boolean isCalled()
    {
      return called;
    }

  }

}
