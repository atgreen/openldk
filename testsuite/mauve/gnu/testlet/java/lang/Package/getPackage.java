// Tags: JDK1.2

// Copyright (C) 2004, 2005 Free Software Foundation, Inc.
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

package gnu.testlet.java.lang.Package;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class getPackage implements Testlet
{
  public void test (TestHarness harness)
    {
      String name = "gnu.testlet.java.lang.Package";
      Package p = Package.getPackage(name);
      if (p != null)
	harness.check(name, p.getName());
      else
	harness.debug("getPackage() returned null");

      p = Package.getPackage("java.lang");
      harness.check(p != null, "checking package for 'java.lang'");
    }
}
