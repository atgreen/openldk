// Copyright (C) 2003, Red Hat, Inc.
// Copyright (C) 2004, Mark Wielaard <mark@klomp.org>
//
// This file is part of Mauve.
//
// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.
//
// Tags: JDK1.2

package gnu.testlet.java.io.FilePermission;

import gnu.testlet.*;
import java.io.FilePermission;
import java.security.Permissions;

public class simple implements Testlet
{
  public void test(TestHarness harness)
  {
    // Test for a classpath regression.
    Permissions p = new Permissions();
    // (The following used to use the bogus action "nothing" ... but
    // the JDK 1.4.2 javadoc makes it clear that only actions "read",
    // "write", "execute" and "delete" are recognized.  And the JDK
    // 1.4.2 implementation throws IllegalArgumentException for an
    // unrecognized action.)
    p.add(new FilePermission("/tmp/p", "read"));
    p.add(new FilePermission("/tmp/p", "read"));

    // Classpath didn't handle dirs without a file separator correctly
    FilePermission fp1 = new FilePermission("/tmp", "read");
    harness.check(fp1.implies(fp1));

    // Test the constructor's checking of its arguments.
    harness.checkPoint("constructor file arg checking");
    try {
      harness.check(new FilePermission(null, "read") == null);
    }
    catch (java.lang.NullPointerException ex) {
      harness.check(true);
    }

    harness.checkPoint("constructor action checking (simple)");
    harness.check(new FilePermission("/tmp/p", "read") != null);
    harness.check(new FilePermission("/tmp/p", "write") != null);
    harness.check(new FilePermission("/tmp/p", "execute") != null);
    harness.check(new FilePermission("/tmp/p", "delete") != null);
    
    harness.checkPoint("constructor action checking (lists)");
    harness.check(new FilePermission("/tmp/p", "read,delete") != null);
    harness.check(new FilePermission("/tmp/p", "read,read") != null);
    harness.check(new FilePermission("/tmp/p", "read,read,read") != null);

    harness.checkPoint("constructor action checking (case)");
    harness.check(new FilePermission("/tmp/p", "Read,DELETE") != null);
    harness.check(new FilePermission("/tmp/p", "rEAD") != null);

    harness.checkPoint("constructor action checking(underspecified)");
    harness.check(new FilePermission("/tmp/p", " read ") != null);
    harness.check(new FilePermission("/tmp/p", "read, read") != null);
    harness.check(new FilePermission("/tmp/p", "read ,read") != null);

    harness.checkPoint("constructor action checking(bad actions)");
    try {
      harness.check(new FilePermission("/tmp/p", null) == null);
    }
    catch (java.lang.IllegalArgumentException ex) {
      harness.check(true);
    }
    try {
      harness.check(new FilePermission("/tmp/p", "") == null);
    }
    catch (java.lang.IllegalArgumentException ex) {
      harness.check(true);
    }
    try {
      harness.check(new FilePermission("/tmp/p", " ") == null);
    }
    catch (java.lang.IllegalArgumentException ex) {
      harness.check(true);
    }
    try {
      harness.check(new FilePermission("/tmp/p", "foo") == null);
    }
    catch (java.lang.IllegalArgumentException ex) {
      harness.check(true);
    }
    try {
      harness.check(new FilePermission("/tmp/p", "nothing") == null);
    }
    catch (java.lang.IllegalArgumentException ex) {
      harness.check(true);
    }

    harness.checkPoint("constructor action checking(bad action lists)");
    try {
      harness.check(new FilePermission("/tmp/p", ",") == null);
    }
    catch (java.lang.IllegalArgumentException ex) {
      harness.check(true);
    }
    try {
      harness.check(new FilePermission("/tmp/p", "read,") == null);
    }
    catch (java.lang.IllegalArgumentException ex) {
      harness.check(true);
    }
    try {
      harness.check(new FilePermission("/tmp/p", "read,,read") == null);
    }
    catch (java.lang.IllegalArgumentException ex) {
      harness.check(true);
    }

    harness.checkPoint("constructor action checking(wierd stuff)");
    try {
      harness.check(new FilePermission("/tmp/p", "read read") == null);
    }
    catch (java.lang.IllegalArgumentException ex) {
      harness.check(true);
    }
    try {
      harness.check(new FilePermission("/tmp/p", "read\nread") == null);
    }
    catch (java.lang.IllegalArgumentException ex) {
      harness.check(true);
    }
    try {
      harness.check(new FilePermission("/tmp/p", "read;read") == null);
    }
    catch (java.lang.IllegalArgumentException ex) {
      harness.check(true);
    }

    harness.checkPoint("implies() action checking");
    for (int i = 1; i < 1 << actions.length; i++) { 
      for (int j = 1; j < 1 << actions.length; j++) {
	FilePermission pi = new FilePermission("/tmp/p", makeActions(i));
	FilePermission pj = new FilePermission("/tmp/p", makeActions(j));

	harness.check(pi.implies(pj) == ((i & j) == j));
      }
    }
  }

  // stuff for implies action checking
  private static String[] actions = {"read", "write", "execute", "delete"};
  private static String makeActions(int mask)
  {
    String result = "";
    for (int i = 0; i < actions.length; i++)
      {
	if ((mask & (1 << i)) != 0)
	  {
	    if (result.length() > 0)
	      result += ",";
	    result += actions[i];
	  }
      }
    return result;
  }
}
