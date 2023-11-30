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

public class traversal2 implements Testlet
{
  public void test (TestHarness harness)
  {
    try {
      harness.checkPoint("setup");

      String[] ways_to_access = new String[] {
	"dir",     // via the directory
	"rlink",   // via a relative link to the directory
	"alink"};  // via an absolute link to the directory

      String[] ways_to_escape = new String[] {
	"..",      // via the directory
	"rlink",   // via a relative link out of the directory
	"alink"};  // via an absolute link out of the directory

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

	File file = new File(tempdir, "file-present");
	harness.check(file.isFile() || file.createNewFile());
	cleanup.add(file);
	  
	file = new File(tempdir, "file-absent");
	harness.check(!file.exists());

	link = new File(testdir, "rlink");
	harness.check(Runtime.getRuntime().exec(new String[] {
	  "ln", "-s", "..", link.getPath()
	  }).waitFor() == 0);
	cleanup.add(link);

	link = new File(testdir, "alink");
	harness.check(Runtime.getRuntime().exec(new String[] {
	  "ln", "-s", tempdir.getPath(), link.getPath()
	  }).waitFor() == 0);
	cleanup.add(link);

	harness.checkPoint("test");
	for (int i = 0; i < ways_to_access.length; i++) {
	  String how_to_access = ways_to_access[i];

	  FilePermission a = new FilePermission(new File(
	    new File(tempdir, how_to_access), "-").getPath(), "read");

	  for (int j = 0; j < ways_to_escape.length; j++) {
	    String how_to_escape = ways_to_escape[j];
	    for (int k = 0; k < item_states.length; k++) {
	      String item = "file-" + item_states[k];

	      FilePermission b = new FilePermission(new File(
	        new File(testdir, how_to_escape), item).getPath(), "read");

	      harness.debug("\na = " + a);
	      harness.debug("b = " + b);

	      harness.check(!a.implies(b));
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
