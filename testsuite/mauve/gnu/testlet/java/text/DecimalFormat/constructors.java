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
import java.util.Locale;

/**
 * Some checks for the constructors in the DecimalFormat class.  
 */
public class constructors implements Testlet
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not allowed).
   */
  public void test(TestHarness harness) 
  {
    testConstructor1(harness);
    testConstructor2(harness);
    testConstructor3(harness);
  }

  private void testConstructor1(TestHarness harness) 
  {
    harness.checkPoint("DecimalFormat();");
    
    // set the default locale to something fixed
    Locale original = Locale.getDefault();
    Locale.setDefault(Locale.UK);
    
    DecimalFormat f = new DecimalFormat();
    
    // should use the default symbols...
    harness.check(f.getPositivePrefix(), "");                   // 1
    harness.check(f.getNegativePrefix(), "-");                  // 2
    harness.check(f.getPositiveSuffix(), "");                   // 3
    harness.check(f.getNegativeSuffix(), "");                   // 4
    harness.check(f.getMultiplier(), 1);                        // 5
    harness.check(f.getGroupingSize(), 3);                      // 6
    harness.check(f.isDecimalSeparatorAlwaysShown(), false);    // 7
    DecimalFormatSymbols symbols = new DecimalFormatSymbols();
    harness.check(f.getDecimalFormatSymbols(), symbols);        // 8
    
    Locale.setDefault(original);
  }
  
  private void testConstructor2(TestHarness harness) 
  {
    harness.checkPoint("DecimalFormat(String);");
    // set the default locale to something fixed
    Locale original = Locale.getDefault();
    Locale.setDefault(Locale.UK);

    DecimalFormat f = new DecimalFormat("0.00");
    
    // check for null format...
    boolean pass = false;
    try
    {
      f = new DecimalFormat(null);
    }
    catch (NullPointerException e)
    {
      pass = true;   
    }
    harness.check(pass);
    
    // check for bad format...
    pass = false;
    try
    {
      f = new DecimalFormat(";;");   
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;   
    }
    harness.check(pass);
    Locale.setDefault(original);
  }
  
  private void testConstructor3(TestHarness harness) 
  {
    harness.checkPoint("DecimalFormat(String, DecimalFormatSymbols);");

    // since get/setDecimalFormatSymbols() methods make copies of the symbols,
    // we can guess that the constructor does too
    DecimalFormatSymbols dfs = new DecimalFormatSymbols();
    dfs.setDecimalSeparator('x');
    DecimalFormat f = new DecimalFormat("0.00", dfs);
    harness.check(f.getDecimalFormatSymbols().getDecimalSeparator(), 'x');
    dfs.setDecimalSeparator('y');  // this won't affect f
    harness.check(f.getDecimalFormatSymbols().getDecimalSeparator(), 'x');
    
    // check for null format...
    boolean pass = false;
    try
    {
      f = new DecimalFormat(null, new DecimalFormatSymbols());
    }
    catch (NullPointerException e)
    {
      pass = true;   
    }
    harness.check(pass);
    
    // check for null symbols...
    pass = false;
    try
    {
      f = new DecimalFormat("0.00", null);   
    }
    catch (NullPointerException e)
    {
      pass = true;   
    }
    harness.check(pass);
  }

}
