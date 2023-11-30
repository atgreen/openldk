// Tags: JDK1.0

// Copyright (C) 1999, 2000, 2001 Cygnus Solutions

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

package gnu.testlet.java.lang.StringBuffer;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

// This test uses the `+' operator to test StringBuffer.
public class plus implements Testlet
{
  public String s (int x)
    {
      if (x == 0)
	return null;
      else
	return "z";
    }

  public void test (TestHarness harness)
    {
      harness.check (s(0) + "", "null");

      harness.check (s(1) + "", "z");

      harness.check ("wxy" + s(0), "wxynull");
      harness.check ("wxy" + s(1), "wxyz");

      harness.check (5 + s(1), "5z");

      harness.check (0.2 + "" + s(0) + 7, "0.2null7");
    }
}
