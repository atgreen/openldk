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

package gnu.testlet.java.io.RandomAccessFile;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FilePermission;
import java.io.IOException;
import java.io.RandomAccessFile;
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
    try {
      new FileOutputStream(file);
    }
    catch (FileNotFoundException e) {
      harness.debug(e);
      harness.check(false, "unexpected exception");
    }

    Permission rperm = new FilePermission(path, "read");
    Permission wperm = new FilePermission(path, "write");
    Permission rfdperm = new RuntimePermission("readFileDescriptor");
    Permission wfdperm = new RuntimePermission("writeFileDescriptor");

    String[] modes = new String[] {"r", "rw", "rws", "rwd"};
    
    TestSecurityManager sm = new TestSecurityManager(harness);
    try {
      sm.install();

      for (int i = 0; i < modes.length; i++) {
	String mode = modes[i];

	Permission[] mustCheck, mayCheck;
	if (mode.equals("r")) {
	  mustCheck = new Permission[] {rperm};
	  mayCheck = new Permission[] {rfdperm};
	}
	else {
	  mustCheck = new Permission[] {rperm, wperm};
	  mayCheck = new Permission[] {rfdperm, wfdperm};
	}

	RandomAccessFile raf;

	// throwpoint: java.io.RandomAccessFile-RandomAccessFile(File, String)
	harness.checkPoint("File constructor, mode = \"" + mode + "\"");
	try {
	  sm.prepareChecks(mustCheck, mayCheck);
	  raf = new RandomAccessFile(file, mode);
	  sm.checkAllChecked();
	  if (mode == "r")
	    ensureUnwritable(harness, raf);
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}

	// throwpoint: java.io.RandomAccessFile-RandomAccessFile(String, String)
	harness.checkPoint("String constructor, mode = \"" + mode + "\"");
	try {
	  sm.prepareChecks(mustCheck, mayCheck);
	  raf = new RandomAccessFile(path, mode);
	  sm.checkAllChecked();
	  if (mode == "r")
	    ensureUnwritable(harness, raf);
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}
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

  private void ensureUnwritable(TestHarness harness, RandomAccessFile file)
  {
    harness.checkPoint("read-only checks");

    byte[] barry = new byte[] {2, 4, 2};
    try {
      for (int i = 1; i <= 14; i++) {
	long pointer = file.getFilePointer();
	try {
	  switch (i) {
	  case 1:
	    file.write(barry);
	    break;
	  case 2:
	    file.write(barry, 1, 2);
	    break;
	  case 3:
	    file.write(1);
	    break;
	  case 4:
	    file.writeBoolean(true);
	    break;
	  case 5:
	    file.writeByte(1);
	    break;
	  case 6:
	    file.writeBytes("hello mum");
	    break;
	  case 7:
	    file.writeChar(1);
	    break;
	  case 8:
	    file.writeChars("hello mum");
	    break;
	  case 9:
	    file.writeDouble(1);
	    break;
	  case 10:
	    file.writeFloat(1);
	    break;
	  case 11:
	    file.writeInt(1);
	    break;
	  case 12:
	    file.writeLong(1);
	    break;
	  case 13:
	    file.writeShort(1);
	    break;
	  case 14:
	    file.writeUTF("hello mum");
	    break;
	  }
	  harness.check(false);
	}
	catch (IOException e) {
	  harness.check(file.getFilePointer() == pointer);
	}
      }
    }
    catch (IOException e) {
      harness.debug(e);
      harness.check(false, "unexpected IOException");
    }
  }
}
