// Copyright (C) 2006, 2007 Red Hat, Inc.
// Written by Gary Benson <gbenson@redhat.com>

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

// Tags: JDK1.2

package gnu.testlet.java.io.ObjectInputStream;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.SerializablePermission;
import java.security.Permission;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import gnu.testlet.TestSecurityManager;

public class security implements Testlet
{
  public void test(TestHarness harness)
  {
    try {
      TestObjectInputStream teststream = new TestObjectInputStream();
  
      Permission[] enableSubclassImplementation = new Permission[] {
	new SerializablePermission("enableSubclassImplementation")};

      Permission[] enableSubstitution = new Permission[] {
	new SerializablePermission("enableSubstitution")};

      Permission[] noPerms = new Permission[] {};

      TestSecurityManager sm = new TestSecurityManager(harness);
      try {
	sm.install();

	// throwpoint: java.io.ObjectInputStream-ObjectInputStream
	harness.checkPoint("constructor");
	try {
	  sm.prepareChecks(enableSubclassImplementation);
	  new TestObjectInputStream();
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}

	// throwpoint: java.io.ObjectInputStream-enableResolveObject
	harness.checkPoint("enableResolveObject");
	try {
	  sm.prepareChecks(noPerms);
	  teststream.testEnableResolveObject(false);
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}
	try {
	  sm.prepareChecks(enableSubstitution);
	  teststream.testEnableResolveObject(true);
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

  private static class TestObjectInputStream extends ObjectInputStream
  {
    public TestObjectInputStream() throws IOException
    {
      super();
    }

    public boolean testEnableResolveObject(boolean enable)
    {
      return enableResolveObject(enable);
    }
  }
}
