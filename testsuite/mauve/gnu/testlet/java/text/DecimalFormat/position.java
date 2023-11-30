// Test FieldPosition parameter to DecimalFormat.format.

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

package gnu.testlet.java.text.DecimalFormat;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.text.DecimalFormat;
import java.util.Locale;
import java.text.FieldPosition;
import java.text.NumberFormat;

public class position implements Testlet
{
  public String format (DecimalFormat df, double number, FieldPosition pos,
			StringBuffer buf)
    {
      buf.setLength (0);
      return df.format (number, buf, pos).toString();
    }

  public void test (TestHarness harness)
    {
      // Just to be explicit: we're only testing the US locale here.
      Locale loc = Locale.US;
      Locale.setDefault (loc);

      StringBuffer buf = new StringBuffer ();
      DecimalFormat df = new DecimalFormat ("0.##");
      FieldPosition intPos = new FieldPosition (NumberFormat.INTEGER_FIELD);
      FieldPosition fracPos = new FieldPosition (NumberFormat.FRACTION_FIELD);

      harness.check (format (df, -1234.56, intPos, buf), "-1234.56");
      harness.check (intPos.getBeginIndex (), 1);
      harness.check (intPos.getEndIndex (), 5);

      harness.check (format (df, -1234.56, fracPos, buf), "-1234.56");
      harness.check (fracPos.getBeginIndex (), 6);
      harness.check (fracPos.getEndIndex (), 8);
    }
}
