/* MaximumAndMinimumDigits.java -- 
   Copyright (C) 2006 Lima Software, SO.PR.IND. s.r.l.
This file is part of Mauve.

Mauve is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Mauve is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Mauve; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

*/

// Tags: JDK1.4

package gnu.testlet.java.text.DecimalFormat;

import java.text.DecimalFormat;
import java.util.Locale;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Test class for maximum and minimum integer and fraction digits getter/setter
 * in class DecimalFormat.
 * 
 * @author Mario Torre <neugens@limasoftware.net>
 */
public class MaximumAndMinimumDigits implements Testlet
{
  /** Test Harness */
  private TestHarness harness = null;
  
  /* 
   * @see gnu.testlet.Testlet#test(gnu.testlet.TestHarness)
   */
  public void test(TestHarness harness)
  {
    this.harness = harness;
    
    Locale original = Locale.getDefault();
    Locale.setDefault(Locale.US);
    
    try
      {
        doTest();
      }
    finally
      {
        Locale.setDefault(original);
      }
  }
  
  private void doTest()
  {
    // this value is 2147483647
    int MAX = Integer.MAX_VALUE;
    
    harness.checkPoint("default pattern");
    DecimalFormat format = new DecimalFormat();
    harness.check(format.getMaximumIntegerDigits(),  MAX);
    harness.check(format.getMinimumIntegerDigits(),  1);
    harness.check(format.getMaximumFractionDigits(), 3);
    harness.check(format.getMinimumFractionDigits(), 0);
    
    harness.checkPoint("0.00E0");
    format = new DecimalFormat("0.00E0");
    harness.check(format.getMaximumIntegerDigits(),  1);
    harness.check(format.getMinimumIntegerDigits(),  1);
    harness.check(format.getMaximumFractionDigits(), 2);
    harness.check(format.getMinimumFractionDigits(), 2);
    
    harness.checkPoint("#,##0.0#");
    format = new DecimalFormat("#,##0.0#");
    harness.check(format.getMaximumIntegerDigits(),  MAX);
    harness.check(format.getMinimumIntegerDigits(),  1);
    harness.check(format.getMaximumFractionDigits(), 2);
    harness.check(format.getMinimumFractionDigits(), 1);
    
    // check what happen if we force a different value
    harness.checkPoint("maximum integer digits, checking format...");
    format.setMaximumIntegerDigits(0);
    harness.check(format.getMaximumIntegerDigits(), 0);
    harness.check(format.format(123456.123456), ".12");
    
    harness.checkPoint("#.");
    format = new DecimalFormat("#.");
    harness.check(format.getMaximumIntegerDigits(),  MAX);
    harness.check(format.getMinimumIntegerDigits(),  1);
    harness.check(format.getMaximumFractionDigits(), 0);
    harness.check(format.getMinimumFractionDigits(), 0);
    
    harness.checkPoint("#.#");
    format = new DecimalFormat("#.#");
    harness.check(format.getMaximumIntegerDigits(),  MAX);
    harness.check(format.getMinimumIntegerDigits(),  1);
    harness.check(format.getMaximumFractionDigits(), 1);
    harness.check(format.getMinimumFractionDigits(), 0);
    
    harness.checkPoint("#0000000000000,00000.###");
    format = new DecimalFormat("#0000000000000,00000.###");
    harness.check(format.getMaximumIntegerDigits(),  MAX);
    harness.check(format.getMinimumIntegerDigits(),  18);
    harness.check(format.getMaximumFractionDigits(), 3);
    harness.check(format.getMinimumFractionDigits(), 0);
    
    harness.checkPoint("0E0");
    format = new DecimalFormat("0E0");
    harness.check(format.getMaximumIntegerDigits(),  1);
    harness.check(format.getMinimumIntegerDigits(),  1);
    harness.check(format.getMaximumFractionDigits(), 0);
    harness.check(format.getMinimumFractionDigits(), 0);
    
    harness.checkPoint("0.###E0");
    format = new DecimalFormat("0.###E0");
    harness.check(format.getMaximumIntegerDigits(),  1);
    harness.check(format.getMinimumIntegerDigits(),  1);
    harness.check(format.getMaximumFractionDigits(), 3);
    harness.check(format.getMinimumFractionDigits(), 0);
    
    harness.checkPoint(".00");
    format = new DecimalFormat(".00");
    harness.check(format.getMaximumIntegerDigits(),  MAX);
    harness.check(format.getMinimumIntegerDigits(),  0);
    harness.check(format.getMaximumFractionDigits(), 2);
    harness.check(format.getMinimumFractionDigits(), 2);
  }
}
