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
import java.util.Locale;

/**
 * Some checks for the applyPattern() method in the {@link DecimalFormat} class.
 */
public class applyPattern implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness) 
  {
    Locale orig = Locale.getDefault();
    Locale.setDefault(Locale.US);
    
    DecimalFormat f1 = new DecimalFormat();
    
    // negativePrefix
    harness.checkPoint("negativePrefix");
    f1.applyPattern("0.00");
    harness.check(f1.getNegativePrefix(), "-");
    f1.applyPattern("0.00;-0.00");
    harness.check(f1.getNegativePrefix(), "-");
    
    // minimumIntegerDigits
    harness.checkPoint("minimumIntegerDigits");
    f1.applyPattern("0.00");
    harness.check(f1.getMinimumIntegerDigits(), 1);
    f1.applyPattern("#0.00");
    harness.check(f1.getMinimumIntegerDigits(), 1);
    f1.applyPattern("00.00");
    harness.check(f1.getMinimumIntegerDigits(), 2);
    
    // minimumFractionDigits
    harness.checkPoint("minimumFractionDigits");
    f1.applyPattern("0.0");
    harness.check(f1.getMinimumFractionDigits(), 1);
    f1.applyPattern("0.0#");
    harness.check(f1.getMinimumFractionDigits(), 1);
    f1.applyPattern("0.00");
    harness.check(f1.getMinimumFractionDigits(), 2);
    
    // grouping
    harness.checkPoint("grouping");
    f1.applyPattern("0.00");
    harness.check(f1.getGroupingSize(), 0);
    f1.applyPattern("#0.00");
    harness.check(f1.getGroupingSize(), 0);
    f1.applyPattern(",#0.00");
    harness.check(f1.getGroupingSize(), 2);
    f1.applyPattern("#,##0.00");
    harness.check(f1.getGroupingSize(), 3);
    f1.applyPattern("#,#,##0.00");
    
    harness.checkPoint("null pattern");
    f1.applyPattern("");
    harness.check(f1.format(123456789.123456789), "123,456,789.12345679");
    
    harness.checkPoint("invalid pattern");
    // try null argument
    boolean pass = false;
    try
    {
      f1.applyPattern(null);
    }
    catch (NullPointerException e)
    {
      pass = true;   
    }
    harness.check(pass);
    
    // try illegal pattern
    pass = false;
    try
    {
      f1.applyPattern(";;");
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);
    
    Locale.setDefault(orig);
  }

}
