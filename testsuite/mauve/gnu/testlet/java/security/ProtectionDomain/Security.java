// Copyright (C) 2006, 2007, 2010 Red Hat, Inc.
// Original written by Gary Benson <gbenson@redhat.com>
// Adapted for ProtectionDomain by Andrew John Hughes <ahughes@redhat.com>

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

import java.net.URL;

import java.security.CodeSource;
import java.security.Permission;
import java.security.ProtectionDomain;
import java.security.SecurityPermission;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import gnu.testlet.TestSecurityManager;

public class Security implements Testlet
{
  public void test(TestHarness harness)
  {
    try {

      Permission[] noPerms = new Permission[] {};
      Permission[] gpPerms =
        new Permission[] { new SecurityPermission("getPolicy") } ;

      String debug = System.getProperty("java.security.debug");
      ProtectionDomain pd = new ProtectionDomain(null, null, null, null);
      TestSecurityManager sm = new TestSecurityManager(harness);
      try {
        sm.install();

        harness.checkPoint("toString");
        try {
          if (debug != null &&
              (debug.contains("domain") || debug.contains("all")))
            sm.prepareChecks(noPerms);
          else
            sm.prepareChecks(gpPerms);
          harness.debug(pd.toString());
          sm.checkAllChecked();
        }
        catch (SecurityException ex) {
          harness.debug(ex);
          harness.check(false, "unexpected check");
        }

      }
      finally {
        sm.uninstall();
      }
    }
    catch (Exception ex) {
      harness.debug(ex);
      harness.check(false, "Unexpected exception");
    }
  }

}
