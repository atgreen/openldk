// Tags: JDK1.2

// Copyright (C) 2005 David Gilbert <david.gilbert@object-refinery.com>

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

/**
 * Some checks for the equals() method in the SimpleDateFormat
 * class.  Bug 5066247 is a general request for a better API specification.
 */
public class equals implements Testlet
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not allowed).
   */
  public void test(TestHarness harness) 
  {
    SimpleDateFormat f1 = new SimpleDateFormat();
    SimpleDateFormat f2 = new SimpleDateFormat();
    harness.check(f1.equals(f2));                  // check 1
    
    f1 = new SimpleDateFormat("yyyy");
    harness.check(!f1.equals(f2));                 // check 2
    f2 = new SimpleDateFormat("yyyy");
    harness.check(f1.equals(f2));                  // check 3
    
    DateFormatSymbols dfs1 = new DateFormatSymbols(Locale.GERMAN);
    DateFormatSymbols dfs2 = new DateFormatSymbols(Locale.ENGLISH);
    f1 = new SimpleDateFormat("yyyy", dfs1);
    f2 = new SimpleDateFormat("yyyy", dfs2);
    harness.check(!f1.equals(f2));                 // check 4
    f2.setDateFormatSymbols(dfs1);
    harness.check(f1.equals(f2));                  // check 5
    
    Date d1 = new Date();
    
    // check null argument
    harness.check(!d1.equals(null));               // check 6
    
    // check arbitrary argument
    harness.check(!d1.equals("Not a SimpleDateFormat"));  // check 7
  }

}
