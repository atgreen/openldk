// Tags: JDK1.0

// Copyright (C) 1998 Cygnus Solutions

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

package gnu.testlet.java.lang.Boolean;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class equals_Boolean implements Testlet
{
  public void test (TestHarness harness)
    {
      Boolean a = new Boolean("true");
      Boolean b = new Boolean("false");
      Integer i = new Integer(123);

      harness.check (! a.equals(null));
      harness.check (! a.equals(b));
      harness.check (a.equals(Boolean.TRUE));
      harness.check (! a.equals(Boolean.FALSE));
      harness.check (b.equals(Boolean.FALSE));
      harness.check (! b.equals(i));
    }
}
