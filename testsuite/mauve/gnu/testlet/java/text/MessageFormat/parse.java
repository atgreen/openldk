// Test simple forms of MessageFormat parsing.

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

package gnu.testlet.java.text.MessageFormat;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.text.MessageFormat;
import java.text.ParsePosition;
import java.util.Locale;

public class parse implements Testlet
{
  public void test (TestHarness harness)
    {
      MessageFormat mf;
      ParsePosition pp = new ParsePosition (0);
      Object[] val;

      // Just to be explicit: we're only testing the US locale here.
      Locale loc = Locale.US;
      Locale.setDefault (loc);

      mf = new MessageFormat ("no variables");
      mf.setLocale (loc);

      harness.checkPoint ("no variables");
      pp.setIndex(0);
      val = mf.parse ("no zardoz", pp);
      harness.check (val, null);

      pp.setIndex(0);
      val = mf.parse ("no variables", pp);
      harness.check (val.length, 0);

      harness.checkPoint ("one variable");
      mf.applyPattern ("I have seen zardoz number {0}.");

      pp.setIndex(0);
      val = mf.parse ("I have seen zardoz number 23.", pp);
      harness.check (val.length, 1);
      harness.check (val[0] instanceof String);
      harness.check ((String) (val[0]), "23");

      harness.checkPoint ("number format");
      mf.applyPattern ("I have seen zardoz number {0,number}!");

      pp.setIndex(0);
      val = mf.parse ("I have seen zardoz number 23!", pp);
      harness.check (val.length, 1);
      harness.check (val[0] instanceof Number);
      harness.check (((Number) (val[0])).longValue (), 23);

      harness.checkPoint ("greedy string matching at end");
      mf.applyPattern ("/foo/{0}");

      pp.setIndex(0);
      val = mf.parse ("/foo/bar", pp);
      harness.check (val.length, 1);
      harness.check (val[0] instanceof String);
      harness.check (val[0], "bar");


    }
}
