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

package gnu.testlet.java.text.DecimalFormat;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;

/**
 * Some checks for the getDecimalFormatSymbols() method in the 
 * {@link DecimalFormat} class.
 */
public class getDecimalFormatSymbols implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness) 
  {
    DecimalFormat f1 = new DecimalFormat();
    
    // returned value is a clone, not the original - see bug parade 4685440
    DecimalFormatSymbols symbols1 = f1.getDecimalFormatSymbols();
    DecimalFormatSymbols symbols2 = f1.getDecimalFormatSymbols();
    harness.check(symbols1 != symbols2);
    harness.check(symbols1.equals(symbols2));
  }

}
