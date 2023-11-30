//Tags: JDK1.4

//Copyright (C) 2004 David Gilbert <david.gilbert@object-refinery.com>

//This file is part of Mauve.

//Mauve is free software; you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation; either version 2, or (at your option)
//any later version.

//Mauve is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.

//You should have received a copy of the GNU General Public License
//along with Mauve; see the file COPYING.  If not, write to
//the Free Software Foundation, 59 Temple Place - Suite 330,
//Boston, MA 02111-1307, USA.

package gnu.testlet.java.lang.Double;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Some tests for the compare() method in the {@link Double} class.
 */
public class compare implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    harness.check(Double.compare(0.0, 1.0) < 0);
    harness.check(Double.compare(0.0, 0.0) == 0);
    harness.check(Double.compare(1.0, 0.0) > 0);

    harness.check(Double.compare(Double.POSITIVE_INFINITY, 0.0) > 0);
    harness.check(Double.compare(Double.POSITIVE_INFINITY, Double.POSITIVE_INFINITY) == 0);
    harness.check(Double.compare(0.0f, Double.POSITIVE_INFINITY) < 0);

    harness.check(Double.compare(Double.NEGATIVE_INFINITY, 0.0) < 0);
    harness.check(Double.compare(Double.NEGATIVE_INFINITY, Double.NEGATIVE_INFINITY) == 0);
    harness.check(Double.compare(0.0, Double.NEGATIVE_INFINITY) > 0);

    harness.check(Double.compare(Double.NaN, 0.0) > 0);
    harness.check(Double.compare(Double.NaN, Double.NaN) == 0);
    harness.check(Double.compare(0.0f, Double.NaN) < 0);
    
    harness.check(Double.compare(0.0, -0.0) > 0);
    harness.check(Double.compare(-0.0, 0.0) < 0);
  }
  
}