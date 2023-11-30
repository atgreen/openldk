// Copyright (C) 2005, 2006, 2007 Red Hat, Inc.
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

package gnu.testlet.java.io.FileInputStream;

import java.io.File;
import java.io.FileDescriptor;
import java.io.FileInputStream;
import java.io.FilePermission;
import java.security.Permission;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import gnu.testlet.TestSecurityManager;

public class security implements Testlet
{
  public void test (TestHarness harness)
  {
    File file = new File(harness.getSourceDirectory(), "ChangeLog");
    String path = file.getPath();
    
    Permission[] perm = new Permission[] {
	new FilePermission(path, "read")};
    Permission[] fdPerm = new Permission[] {
	new RuntimePermission("readFileDescriptor")};
    
    TestSecurityManager sm = new TestSecurityManager(harness);
    try {
      sm.install();
	
      // throwpoint: java.io.FileInputStream-FileInputStream(File)
      harness.checkPoint("File constructor");
      try {
	sm.prepareChecks(perm);
	new FileInputStream(file);
	sm.checkAllChecked();
      }
      catch (SecurityException ex) {
	harness.debug(ex);
	harness.check(false, "Unexpected check");
      }
      
      // throwpoint: java.io.FileInputStream-FileInputStream(String)
      harness.checkPoint("String constructor");
      try {
	sm.prepareChecks(perm);
	new FileInputStream(path);
	sm.checkAllChecked();
      }
      catch (SecurityException ex) {
	harness.debug(ex);
	harness.check(false, "Unexpected check");
      }
      
      // throwpoint: java.io.FileInputStream-FileInputStream(FileDescriptor)
      harness.checkPoint("FileDescriptor constructor");
      try {
	sm.prepareChecks(fdPerm);
	new FileInputStream(FileDescriptor.in);
	sm.checkAllChecked();
      }
      catch (SecurityException ex) {
	harness.debug(ex);
	harness.check(false, "Unexpected check");
      }
    }
    catch (Exception ex) {
      harness.debug(ex);
      harness.check(false, "Unexpected exception");
    }
    finally {
	sm.uninstall();
    }
  }
}
