// Copyright (C) 2011 Red Hat, Inc.
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

// Tags: JDK1.5

package gnu.testlet.java.security.Policy;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.security.Permission;
import java.security.Permissions;
import java.security.PermissionCollection;
import java.security.Policy;
import java.security.ProtectionDomain;
import java.security.SecurityPermission;

/**
 * Tests whether the toString output of ProtectionDomain
 * includes the Policy's protection domains when the
 * SecurityManager denies the getPolicy permission.
 * See PR42390.
 */
public class Security implements Testlet
{

  public void test(TestHarness harness)
  {
    Policy.setPolicy(new Policy()
      {
        public PermissionCollection getPermissions(ProtectionDomain domain)
        {
          Permissions perms = new Permissions();
          perms.add(new TestPermission());
          return perms;
        }
      });
    ProtectionDomain pd = Security.class.getProtectionDomain();
    System.setSecurityManager(new SecurityManager()
      {
        public void checkPermission(Permission perm)
        {
          if ((perm instanceof SecurityPermission) &&
              perm.getName().equals("getPolicy"))
            {
              throw new SecurityException("Policy retrieval disallowed.");
            }
        }
      });
    String testPermString = new TestPermission().toString();
    harness.check(!pd.toString().contains(testPermString),
                  "Policy permissions should not be visible");
  }

  private static class TestPermission extends Permission
  {
    public TestPermission()
    {
      super("test");
    }

    public String getActions()
    {
      return "test";
    }

    public int hashCode()
    {
      return "test".hashCode();
    }

    public boolean equals(Object other)
    {
      if (other == null)
        return false;
      if (other == this)
        return true;
      if (other.getClass().equals(getClass()))
        return true;
      return false;
    }

    public boolean implies(Permission permission)
    {
      if (permission instanceof TestPermission)
        return true;
      return false;
    }

  }


}
