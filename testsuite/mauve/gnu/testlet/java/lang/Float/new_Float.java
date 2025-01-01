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

package gnu.testlet.java.lang.Float;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class new_Float implements Testlet
{
  public void test (TestHarness harness)
    {
      // A few simple test cases.
      harness.check (new Float(22.0/7.0).toString (), "3.142857");
      harness.check (new Float(Math.PI).toString (), "3.1415927");
      harness.check (Float.valueOf (new Float(Math.PI).toString ()),
		     new Float(Math.PI));
      harness.check (Float.valueOf (new Float(-Math.PI).toString ()),
		     new Float(-Math.PI));
      harness.check (new Float(Float.MAX_VALUE).toString (), 
		     "3.4028235E38");
      harness.check (new Float(-Float.MAX_VALUE).toString (), 
		     "-3.4028235E38");
      harness.check (new Float(Float.POSITIVE_INFINITY).toString (), 
		     "Infinity");
      harness.check (new Float(-Float.POSITIVE_INFINITY).toString (), 
		     "-Infinity");
      harness.check (new Float(Float.NaN).toString (), "NaN");
    }
}

