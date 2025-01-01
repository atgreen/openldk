// Test ChoiceFormat parsing.

// Copyright (c) 1999  Cygnus Solutions
// Written by Tom Tromey <tromey@cygnus.com>

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

// Tags: JDK1.1

package gnu.testlet.java.text.ChoiceFormat;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.text.ChoiceFormat;
import java.text.ParsePosition;

public class parse implements Testlet
{
  public void test (TestHarness harness)
    {
      ChoiceFormat cf = new ChoiceFormat ("1.0#Sun|2.0#Mon|3.0#Tue|4.0#Wed|5.0#Thu|6.0#Fri|7.0#Sat");
      ParsePosition pp = new ParsePosition (0);

      Number n = cf.parse ("Wed", pp);
      harness.check (n instanceof Double);
      harness.check (n.doubleValue (), 4.0);
      harness.check (pp.getIndex (), 3);

      pp.setIndex (3);
      n = cf.parse ("ZooMon", pp);
      harness.check (n.doubleValue (), 2.0);
      harness.check (pp.getIndex (), 6);

      pp.setIndex (0);
      n = cf.parse ("Saturday", pp);
      harness.check (n.doubleValue (), 7.0);
      harness.check (pp.getIndex (), 3);

      n = cf.parse ("Saturday", pp);
      harness.check (Double.isNaN (n.doubleValue ()));
      harness.check (pp.getIndex (), 3);
    }
}
