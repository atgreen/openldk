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

/**
 * Some checks for the equals() method in the DecimalFormat class.  
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
    DecimalFormat f1 = new DecimalFormat();
    DecimalFormat f2 = new DecimalFormat();
    harness.check(f1.equals(f2));                          // 1
    harness.check(f2.equals(f1));                          // 2
    
    f1.applyPattern("#,##0");
    f2.applyPattern("#,##0.00");
    harness.check(!f1.equals(f2));                         // 3
    f2.applyPattern("#,##0");
    harness.check(f1.equals(f2));                          // 4
    
    f1.setDecimalSeparatorAlwaysShown(!f1.isDecimalSeparatorAlwaysShown());
    harness.check(!f1.equals(f2));                         // 5
    f2.setDecimalSeparatorAlwaysShown(f1.isDecimalSeparatorAlwaysShown());
    harness.check(f1.equals(f2));                          // 6
    
    f1.setGroupingSize(5);
    harness.check(!f1.equals(f2));                         // 7
    f2.setGroupingSize(5);
    harness.check(f1.equals(f2));                          // 8
    
    f1.setGroupingUsed(!f1.isGroupingUsed());             
    harness.check(!f1.equals(f2));                         // 9
    f2.setGroupingUsed(f1.isGroupingUsed());
    harness.check(f1.equals(f2));                          // 10
    
    f1.setMaximumFractionDigits(12);
    harness.check(!f1.equals(f2));                         // 11
    f2.setMaximumFractionDigits(12);
    harness.check(f1.equals(f2));                          // 12
    
    f1.setMaximumIntegerDigits(23);
    harness.check(!f1.equals(f2));                         // 13
    f2.setMaximumIntegerDigits(23);
    harness.check(f1.equals(f2));                          // 14
    
    f1.setMinimumFractionDigits(5);
    harness.check(!f1.equals(f2));                         // 15
    f2.setMinimumFractionDigits(5);
    harness.check(f1.equals(f2));                          // 16
    
    f1.setMinimumIntegerDigits(4);
    harness.check(!f1.equals(f2));                         // 17
    f2.setMinimumIntegerDigits(4);
    harness.check(f1.equals(f2));                          // 18
    
    f1.setMultiplier(17);
    harness.check(!f1.equals(f2));                         // 19
    f2.setMultiplier(17);
    harness.check(f1.equals(f2));                          // 20
    
    f1.setNegativePrefix("ABC");
    harness.check(!f1.equals(f2));                         // 21
    f2.setNegativePrefix("ABC");
    harness.check(f1.equals(f2));                          // 22
    
    f1.setPositivePrefix("XYZ");
    harness.check(!f1.equals(f2));                         // 23
    f2.setPositivePrefix("XYZ");
    harness.check(f1.equals(f2));                          // 24
    
    f1.setNegativeSuffix("FGH");
    harness.check(!f1.equals(f2));                         // 25
    f2.setNegativeSuffix("FGH");
    harness.check(f1.equals(f2));                          // 26
    
    f1.setPositiveSuffix("JKL");
    harness.check(!f1.equals(f2));                         // 27
    f2.setPositiveSuffix("JKL");
    harness.check(f1.equals(f2));                          // 28
    
    // check equivalent patterns
    f1.applyPattern("0.00");
    f2.applyPattern("0.00;-0.00");
    harness.check(f1.equals(f2));                          // 29
    

    // check null
    harness.check(!f1.equals(null));                       // 30
    
    // check arbitrary object
    harness.check(!f1.equals("Not a DecimalFormat"));      // 31

  }

}
