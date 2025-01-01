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

package gnu.testlet.java.lang.Double;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class new_Double implements Testlet
{
  public void test (TestHarness harness)
    {
      // Some broken implementations convert "7.79625120912E289" to
      // the value 7.796251209119999E289.
      harness.check (new Double("7.79625120912E289").toString (),
		     "7.79625120912E289");

      // A few simple test cases.
      harness.check (new Double(22.0/7.0).toString (), "3.142857142857143");
      harness.check (new Double(Math.PI).toString (), "3.141592653589793");
      harness.check (Double.valueOf (new Double(Math.PI).toString ()),
		     new Double(Math.PI));
      harness.check (Double.valueOf (new Double(-Math.PI).toString ()),
		     new Double(-Math.PI));
      harness.check (new Double(Double.MAX_VALUE).toString (), 
		     "1.7976931348623157E308");
      harness.check (new Double(-Double.MAX_VALUE).toString (), 
		     "-1.7976931348623157E308");
      harness.check (new Double(Double.POSITIVE_INFINITY).toString (), 
		     "Infinity");
      harness.check (new Double(-Double.POSITIVE_INFINITY).toString (), 
		     "-Infinity");
      harness.check (new Double(Double.NaN).toString (), 
		     "NaN");
    }
}

