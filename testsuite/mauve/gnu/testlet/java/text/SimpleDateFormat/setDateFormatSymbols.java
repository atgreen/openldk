// Tags: JDK1.2

// Copyright (C) 2004 David Gilbert <david.gilbert@object-refinery.com>

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

package gnu.testlet.java.text.SimpleDateFormat;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.text.DateFormatSymbols;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

/**
 * Some checks for the setDateFormatSymbols() method in the SimpleDateFormat
 * class.  
 */
public class setDateFormatSymbols implements Testlet
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not allowed).
   */
  public void test(TestHarness harness)
  {
    // check that changing the short weekdays does work...
    SimpleDateFormat sdf = new SimpleDateFormat("E", Locale.UK);
    sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
    Date jan1_2005 = new Date(1104537600000L);
    harness.check(sdf.format(jan1_2005), "Sat");
    DateFormatSymbols s = sdf.getDateFormatSymbols();
    s.setShortWeekdays(new String[] {"-", "S", "M", "T", "W", "T", "F", "S"});
    // remember s is just a copy of the original
    sdf.setDateFormatSymbols(s);
    harness.check(sdf.format(jan1_2005), "S");
    
    // check null argument - it isn't mentioned in the spec that this
    // should throw a NullPointerException, but that is such common
    // behaviour elsewhere that I'm assuming this is the expected 
    // result
    try
    {
      sdf.setDateFormatSymbols(null);
      harness.check(false);
    }
    catch (NullPointerException e)
    {
      harness.check(true);
    }
  }

}
