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

package gnu.testlet.java.security.ProtectionDomain;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.security.AllPermission;
import java.security.CodeSource;
import java.security.Permission;
import java.security.Permissions;
import java.security.PermissionCollection;
import java.security.Policy;
import java.security.ProtectionDomain;
import java.security.SecurityPermission;

/**
 * Tests whether Policy.implies(Permission) is called
 * when the domain has AllPermission as one of its permissions.
 */
public class Implies implements Testlet
{

  private boolean called = false;

  public void test(TestHarness harness)
  {
    Policy.setPolicy(new Policy()
      {
        public boolean implies(ProtectionDomain domain,
                               Permission perm)
        {
          if (perm.getName().equals("TestPermission"))
            called = true;
          return true;
        }
        public void refresh() {}
        public PermissionCollection getPermissions(CodeSource codesource)
        {
          return null;
        }
      });

    System.setSecurityManager(new SecurityManager());
    PermissionCollection coll = new Permissions();
    coll.add(new AllPermission());
    ProtectionDomain pd = new ProtectionDomain(null, coll,
                                               Implies.class.getClassLoader(), null);
    pd.implies(new SecurityPermission("TestPermission"));
    harness.check(!called, "Policy was not checked due to AllPermission");
  }

}
