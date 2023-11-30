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

package gnu.testlet.java.lang.Float;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Some tests for the compare() method in the {@link Float} class.
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
    harness.check(Float.compare(0.0f, 1.0f) < 0);
    harness.check(Float.compare(0.0f, 0.0f) == 0);
    harness.check(Float.compare(1.0f, 0.0f) > 0);

    harness.check(Float.compare(Float.POSITIVE_INFINITY, 0.0f) > 0);
    harness.check(Float.compare(Float.POSITIVE_INFINITY, Float.POSITIVE_INFINITY) == 0);
    harness.check(Float.compare(0.0f, Float.POSITIVE_INFINITY) < 0);

    harness.check(Float.compare(Float.NEGATIVE_INFINITY, 0.0f) < 0);
    harness.check(Float.compare(Float.NEGATIVE_INFINITY, Float.NEGATIVE_INFINITY) == 0);
    harness.check(Float.compare(0.0f, Float.NEGATIVE_INFINITY) > 0);

    harness.check(Float.compare(Float.NaN, 0.0f) > 0);
    harness.check(Float.compare(Float.NaN, Float.NaN) == 0);
    harness.check(Float.compare(0.0f, Float.NaN) < 0);
    
    harness.check(Float.compare(0.0f, -0.0f) > 0);
    harness.check(Float.compare(-0.0f, 0.0f) < 0);
  }
}