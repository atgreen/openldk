// Test simple forms of DecimalFormat.format.

// Copyright (c) 2003 Free Software Foundation, Inc.
// Written by Mark Wielaard <mark@klomp.org>

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

// Tags: JDK1.2

package gnu.testlet.java.text.DecimalFormat;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.text.DecimalFormat;

public class digits implements Testlet
{
  public void test (TestHarness harness)
    {
      DecimalFormat df = new DecimalFormat ();
      df.setMaximumFractionDigits(1);
      harness.check(df.getMaximumFractionDigits(), 1);
      df.setMaximumFractionDigits(350);
      harness.check(df.getMaximumFractionDigits(), 350);

      df.setMinimumFractionDigits(1);
      harness.check(df.getMinimumFractionDigits(), 1);
      df.setMinimumFractionDigits(350);
      harness.check(df.getMinimumFractionDigits(), 350);

      df.setMinimumFractionDigits(16);
      df.setMaximumFractionDigits(12);
      harness.check(df.getMinimumFractionDigits(), 12);

      df.setMaximumFractionDigits(12);
      df.setMinimumFractionDigits(16);
      harness.check(df.getMinimumFractionDigits(), 16);

      df.setMaximumIntegerDigits(1);
      harness.check(df.getMaximumIntegerDigits(), 1);
      df.setMaximumIntegerDigits(310);
      harness.check(df.getMaximumIntegerDigits(), 310);

      df.setMinimumIntegerDigits(1);
      harness.check(df.getMinimumIntegerDigits(), 1);
      df.setMinimumIntegerDigits(310);
      harness.check(df.getMinimumIntegerDigits(), 310);

      df.setMinimumIntegerDigits(16);
      df.setMaximumIntegerDigits(12);
      harness.check(df.getMinimumIntegerDigits(), 12);

      df.setMaximumIntegerDigits(12);
      df.setMinimumIntegerDigits(16);
      harness.check(df.getMinimumIntegerDigits(), 16);
    }
}
