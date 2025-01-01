// Copyright (C) 2006, 2007, 2010 Red Hat, Inc.
// Original written by Gary Benson <gbenson@redhat.com>
// Adapted for LogManager by Andrew John Hughes <ahughes@redhat.com>

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

package gnu.testlet.java.util.logging.LogManager;

import java.security.Permission;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import java.util.logging.LogManager;
import java.util.logging.LoggingPermission;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import gnu.testlet.TestSecurityManager;

public class Security implements Testlet
{
  public void test(TestHarness harness)
  {
    try {

      Permission[] noPerms = new Permission[] {};
      Permission[] logPerms =
        new Permission[] { new LoggingPermission("control", null) } ;

      LogManager lm = LogManager.getLogManager();
      PropertyChangeListener listener = new TestPropertyChangeListener();
      TestSecurityManager sm = new TestSecurityManager(harness);
      try {
        sm.install();

        harness.checkPoint("addPropertyChangeListener");
        try {
          sm.prepareChecks(logPerms);
          lm.addPropertyChangeListener(listener);
          sm.checkAllChecked();
        }
        catch (SecurityException ex) {
          harness.debug(ex);
          harness.check(false, "unexpected check");
        }

        harness.checkPoint("removePropertyChangeListener");
        try {
          sm.prepareChecks(logPerms);
          lm.removePropertyChangeListener(listener);
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

  private static class TestPropertyChangeListener
    implements PropertyChangeListener
  {
    public void propertyChange(PropertyChangeEvent event)
    {
    }
  }

}
