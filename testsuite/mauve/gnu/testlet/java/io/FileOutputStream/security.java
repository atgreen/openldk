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

package gnu.testlet.java.io.FileOutputStream;

import java.io.File;
import java.io.FileDescriptor;
import java.io.FileOutputStream;
import java.io.FilePermission;
import java.security.Permission;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import gnu.testlet.TestSecurityManager;

public class security implements Testlet
{
  public void test (TestHarness harness)
  {
    File dir = new File(harness.getTempDirectory(), "mauve-testdir");
    harness.check(dir.mkdir() || dir.exists(), "temp directory");
    
    File file = new File(dir, "file");
    String path = file.getPath();

    Permission rperm = new FilePermission(path, "read");
    Permission wperm = new FilePermission(path, "write");
    Permission fdPerm = new RuntimePermission("writeFileDescriptor");
    
    TestSecurityManager sm = new TestSecurityManager(harness);
    try {
      sm.install();
	
      // throwpoint: java.io.FileOutputStream-FileOutputStream(File)
      harness.checkPoint("File constructor");
      try {
	sm.prepareChecks(new Permission[] {wperm}, new Permission[] {rperm});
	new FileOutputStream(file);
	sm.checkAllChecked();
      }
      catch (SecurityException ex) {
	harness.debug(ex);
	harness.check(false, "Unexpected check");
      }

      // throwpoint: java.io.FileOutputStream-FileOutputStream(File, boolean)
      harness.checkPoint("File, boolean constructor");
      for (int i = 0; i <= 1; i++) {
	try {
	  sm.prepareChecks(new Permission[] {wperm}, new Permission[] {rperm});
	  new FileOutputStream(file, i == 1);
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "Unexpected check");
	}
      }

      // throwpoint: java.io.FileOutputStream-FileOutputStream(String)
      harness.checkPoint("String constructor");
      try {
	sm.prepareChecks(new Permission[] {wperm}, new Permission[] {rperm});
	new FileOutputStream(path);
	sm.checkAllChecked();
      }
      catch (SecurityException ex) {
	harness.debug(ex);
	harness.check(false, "Unexpected check");
      }

      // throwpoint: java.io.FileOutputStream-FileOutputStream(String, boolean)
      harness.checkPoint("String, boolean constructor");
      for (int i = 0; i <= 1; i++) {
	try {
	  sm.prepareChecks(new Permission[] {wperm}, new Permission[] {rperm});
	  new FileOutputStream(path, i == 1);
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "Unexpected check");
	}
      }

      // throwpoint: java.io.FileOutputStream-FileOutputStream(FileDescriptor)
      harness.checkPoint("FileDescriptor constructor");
      try {
	sm.prepareChecks(new Permission[] {fdPerm});
	new FileOutputStream(FileDescriptor.out);
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

	file.delete();
	dir.delete();
    }
  }
}
