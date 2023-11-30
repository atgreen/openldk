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

package gnu.testlet.java.io.FilePermission;

import java.io.File;
import java.io.FilePermission;
import java.util.LinkedList;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class traversal implements Testlet
{
  public void test (TestHarness harness)
  {
    try {
      harness.checkPoint("setup");

      String[] items_to_access = new String[] {
	"file",    // a file in the directory
	"rlink",   // a relative link to a file outside the directory
	"alink"};  // an absolute link to a file outside the directory

      String[] ways_to_access = new String[] {
	"dir",     // via the directory
	"rlink",   // via a relative link to the directory
	"alink"};  // via an absolute link to the directory

      String[] item_states = new String[] {
	"present", // the file exists
	"absent"}; // the file does not exist

      LinkedList cleanup = new LinkedList();
      try {
	File tempdir = new File(harness.getTempDirectory(), "mauve-testdir");
	harness.check(tempdir.isDirectory() || tempdir.mkdir());
	cleanup.add(tempdir);

	File testdir = new File(tempdir, "dir");
	harness.check(testdir.isDirectory() || testdir.mkdir());
	cleanup.add(testdir);

	File link = new File(tempdir, "rlink");
	harness.check(Runtime.getRuntime().exec(new String[] {
	  "ln", "-s", testdir.getName(), link.getPath()
	  }).waitFor() == 0);
	cleanup.add(link);

	link = new File(tempdir, "alink");
	harness.check(Runtime.getRuntime().exec(new String[] {
	  "ln", "-s", testdir.getPath(), link.getPath()
	  }).waitFor() == 0);
	cleanup.add(link);

	File[] dirs = new File[] {testdir, tempdir};
	for (int i = 0; i < dirs.length; i++) {
	  File file = new File(dirs[i], "file-present");
	  harness.check(file.isFile() || file.createNewFile());
	  cleanup.add(file);

	  file = new File(dirs[i], "file-absent");
	  harness.check(!file.exists());
	}

	for (int i = 0; i < item_states.length; i++) {
	  File file = new File(tempdir, "file-" + item_states[i]);
	  
	  link = new File(testdir, "rlink-" + item_states[i]);
	  harness.check(Runtime.getRuntime().exec(new String[] {
	    "ln", "-s", new File("..",file.getName()).getPath(), link.getPath()
	  }).waitFor() == 0);
	  cleanup.add(link);

	  link = new File(testdir, "alink-" + item_states[i]);
	  harness.check(Runtime.getRuntime().exec(new String[] {
	    "ln", "-s", file.getPath(), link.getPath()
	  }).waitFor() == 0);
	  cleanup.add(link);
	}

	harness.checkPoint("test");
	for (int i = 0; i < items_to_access.length; i++) {
	  String item_to_access = items_to_access[i];
	  for (int j = 0; j < ways_to_access.length; j++) {
	    String how_to_access = ways_to_access[j];
	    for (int k = 0; k < ways_to_access.length; k++) {
	      String how_permitted = ways_to_access[k];
	      for (int l = 0; l < item_states.length; l++) {
		String item_state = item_states[l];
		String item = item_to_access + "-" + item_state;

		FilePermission a = new FilePermission(new File(
		  new File(tempdir, how_permitted), item).getPath(), "read");

		FilePermission b = new FilePermission(new File(
		  new File(tempdir, how_to_access), item).getPath(), "read");

		harness.debug("\na = " + a);
		harness.debug("b = " + b);

		harness.check(a.implies(b));
	      }
	    }
	  }
	}
      }
      finally {
	for (int i = cleanup.size() - 1; i >= 0; i--)
	  ((File) cleanup.get(i)).delete();
      }
    }
    catch (Throwable ex) {
      harness.debug(ex);
      harness.check(false, "Unexpected exception");
    }
  }
}
