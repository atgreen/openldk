// Test exponential forms of DecimalFormat.format.

// Copyright (c) 1999, 2003  Cygnus Solutions
// Written by Tom Tromey <tromey@cygnus.com>

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
import java.util.Locale;

// Some 1.1 versions allowed 'E', but only 1.2 defines it officially.
public class formatExp implements Testlet
{
  public void apply (TestHarness harness, DecimalFormat df, String pattern)
    {
      harness.checkPoint("pattern " + pattern);
      boolean ok = true;
      try
	{
	  df.applyPattern(pattern);
	}
      catch (IllegalArgumentException x)
	{
	  ok = false;
	}
      harness.check (ok);
    }

  public void test (TestHarness harness)
    {
      // Just to be explicit: we're only testing the US locale here.
      Locale loc = Locale.US;
      Locale.setDefault (loc);
      DecimalFormat df = new DecimalFormat();

      apply (harness, df, "0.0000E0");
      harness.check (df.format (200000), "2.0000E5");

      apply (harness, df, "00.00E00");
      harness.check (df.format (200000), "20.00E04");

      apply (harness, df, "##0.####E0");
      harness.check (df.format (12345), "12.345E3");

      apply (harness, df, "##.###E0");
      harness.check (df.format (12345), "1.2345E4");

      apply (harness, df, "##.###E0");
      harness.check (df.format (12346), "1.2346E4");
      
      apply (harness, df, "00.###E0");
      harness.check (df.format (12345), "12.345E3");
      harness.check (df.format (1234), "12.34E2");
      harness.check (df.format (0.00123), "12.3E-4");
      
      apply(harness, df, "0E0");
      harness.check(df.format(-1234.567), "-1E3");
      
      apply(harness, df, "00E00");
      harness.check(df.format(-1234.567), "-12E02");
      
      apply(harness, df, "000E00");
      harness.check(df.format(-1234.567), "-123E01");
      
      apply(harness, df, "0000000000E0");
      harness.check(df.format(-1234.567), "-1234567000E-6");
     
      apply(harness, df, "0.0E0");
      harness.check(df.format(-1234.567), "-1.2E3");
      
      apply(harness, df, "00.00E0");
      harness.check(df.format(-1234.567), "-12.35E2");
      harness.check(df.format(-.1234567), "-12.35E-2");
    }
}
