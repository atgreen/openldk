// Tags: JDK1.2

// Copyright (C) 2006 Free Software Foundation
// Contributed by Mark Wielaard (mark@klomp.org)

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
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.security.BasicPermission;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.security.*;

public class newPermission extends BasicPermission implements Testlet
{
  public void test (TestHarness harness)
  {
    Permission p;
    p = new newPermission("a");
    harness.check("a", p.getName());
    p = new newPermission("a", "b");
    harness.check("a", p.getName());
    harness.check("b", p.getActions());

    boolean exception_thrown;
    try
      {
	p = new newPermission("");
	exception_thrown = false;
      }
    catch (IllegalArgumentException iae)
      {
	exception_thrown = true;
      }
    harness.check(exception_thrown);

    try
      {
	p = new newPermission(null);
	exception_thrown = false;
      }
    catch (NullPointerException npe)
      {
	exception_thrown = true;
      }
    harness.check(exception_thrown);

    try
      {
	p = new newPermission("", "");
	exception_thrown = false;
      }
    catch (IllegalArgumentException iae)
      {
	exception_thrown = true;
      }
    harness.check(exception_thrown);

    try
      {
	p = new newPermission(null, "");
	exception_thrown = false;
      }
    catch (NullPointerException npe)
      {
	exception_thrown = true;
      }
    harness.check(exception_thrown);

    try
      {
	p = new newPermission(null, null);
	exception_thrown = false;
      }
    catch (NullPointerException npe)
      {
	exception_thrown = true;
      }
    harness.check(exception_thrown);
  }

  private String actions;
  public newPermission() { super("newPermission"); }
  public newPermission(String n) { super(n); }
  public newPermission(String n, String a) { super(n, a); this.actions = a; }

  public String getActions()
  {
    // BasicPermission.getActions() should return the empty string.
    return super.getActions() + actions;
  }
}
