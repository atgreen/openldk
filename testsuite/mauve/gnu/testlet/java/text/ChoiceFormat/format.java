// Test ChoiceFormat formatting.

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

/**
 * @author John Leuner
 * @author Tom Tromey
 */

public class format implements Testlet
{
  public final String doformat (ChoiceFormat cf, double d, StringBuffer buf)
    {
      buf.setLength (0);
      cf.format (d, buf, null);
      return buf.toString();
    }

  public void test (TestHarness harness)
    {
      StringBuffer buf = new StringBuffer ();

      ChoiceFormat cf = new ChoiceFormat ("1.0#Sun|2.0#Mon|3.0#Tue|4.0#Wed|5.0#Thu|6.0#Fri|7.0#Sat");
      harness.check (cf.getFormats ().length, 7);
      harness.check (cf.getLimits ().length, 7);
      harness.check (doformat (cf, -9, buf), "Sun");
      harness.check (doformat (cf, 1.5, buf), "Sun");
      harness.check (doformat (cf, 5.5, buf), "Thu");
      harness.check (doformat (cf, 7.0, buf), "Sat");
      harness.check (doformat (cf, 99.5, buf), "Sat");

      cf.applyPattern ("-1.0#Less than one|1.0#One|1.0<One to two, exclusive|2.0#Two to three, inclusive|3.0<Over three, up to four|4.0<Four to five, exclusive|5.0#Five and above");
      harness.check (doformat (cf, -23, buf), "Less than one");
      harness.check (doformat (cf, -.5, buf), "Less than one");
      harness.check (doformat (cf, ChoiceFormat.previousDouble (1.0), buf),
		     "Less than one");
      harness.check (doformat (cf, 1.0, buf), "One");
      harness.check (doformat (cf, ChoiceFormat.nextDouble (1.0), buf),
		     "One to two, exclusive");
      harness.check (doformat (cf, 1.5, buf), "One to two, exclusive");
      harness.check (doformat (cf, 2.0, buf), "Two to three, inclusive");
      harness.check (doformat (cf, 3.5, buf), "Over three, up to four");
      harness.check (doformat (cf, 4.0, buf), "Over three, up to four");
      harness.check (doformat (cf, 5.0, buf), "Five and above");
      harness.check (doformat (cf, Double.POSITIVE_INFINITY, buf),
		     "Five and above");
      harness.check (doformat (cf, Double.NaN, buf), "Less than one");
    }
}
