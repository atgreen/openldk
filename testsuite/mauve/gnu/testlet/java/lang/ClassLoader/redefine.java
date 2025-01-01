// Tags: JDK1.2

// Copyright (C) 2005 Free Software Foundation, Inc.
// Written by Tom Tromey <tromey@redhat.com>

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

package gnu.testlet.java.lang.ClassLoader;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class redefine extends ClassLoader implements Testlet
{
  public static class Inner { }

  public static final String INNER_NAME
    = "gnu.testlet.java.lang.ClassLoader.redefine$Inner";

  public void test (TestHarness harness)
  {
    // First try to define a class with the given name.
    boolean ok = false;
    try
      {
	// This call will fail.
	defineClass(INNER_NAME, new byte[37], 0, 37);
      }
    catch (ClassFormatError _)
      {
	ok = true;
      }
    harness.check(ok, "defineClass with invalid parameter");

    // Now load the class normally.
    Class k = null;
    try
      {
	k = Class.forName(INNER_NAME, true, this);
      }
    catch (ClassNotFoundException _)
      {
	// Nothing needed.
      }
    harness.check("" + k, "class " + INNER_NAME, "normal loading");
  }
}
