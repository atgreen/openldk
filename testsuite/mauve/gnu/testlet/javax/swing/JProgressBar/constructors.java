// Tags: JDK1.2

// Copyright (C) 2005 David Gilbert <david.gilbert@object-refinery.com>

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

package gnu.testlet.javax.swing.JProgressBar;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.BoundedRangeModel;
import javax.swing.DefaultBoundedRangeModel;
import javax.swing.JProgressBar;

/**
 * Some checks for the constructors in the {@link JProgressBar} class.
 */
public class constructors implements Testlet {

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    constructor1(harness);
    constructor2(harness);
    constructor3(harness);
    constructor4(harness);
    constructor5(harness);
  }

  public void constructor1(TestHarness harness) 
  {
    harness.checkPoint("JProgressBar()");
    JProgressBar bar = new JProgressBar();
    harness.check(bar.getOrientation(), JProgressBar.HORIZONTAL);
    harness.check(bar.getValue(), 0);
    harness.check(bar.getMinimum(), 0);
    harness.check(bar.getMaximum(), 100);
    harness.check(bar.getString(), "0%");
    harness.check(bar.isStringPainted(), false);
  }

  public void constructor2(TestHarness harness) 
  {
    harness.checkPoint("JProgressBar(BoundedRangeModel)");
    BoundedRangeModel m = new DefaultBoundedRangeModel(50, 10, 0, 100);
    JProgressBar bar = new JProgressBar(m);
    harness.check(bar.getOrientation(), JProgressBar.HORIZONTAL);
    harness.check(bar.getValue(), 50);
    harness.check(bar.getMinimum(), 0);
    harness.check(bar.getMaximum(), 100);
    harness.check(bar.getString(), "50%");
    harness.check(bar.isStringPainted(), false);
    
    boolean pass = false;
    try
    {
      bar = new JProgressBar(null);
    }
    catch (NullPointerException e)
    {
      harness.check(pass);
    }
  }

  public void constructor3(TestHarness harness)  
  {
    harness.checkPoint("JProgressBar(int)");
    JProgressBar bar = new JProgressBar(JProgressBar.HORIZONTAL);
    harness.check(bar.getOrientation(), JProgressBar.HORIZONTAL);
    harness.check(bar.getValue(), 0);
    harness.check(bar.getMinimum(), 0);
    harness.check(bar.getMaximum(), 100);
    harness.check(bar.getString(), "0%");
    harness.check(bar.isStringPainted(), false);
    
    bar = new JProgressBar(JProgressBar.VERTICAL);
    harness.check(bar.getOrientation(), JProgressBar.VERTICAL);

    boolean pass = false;
    try
    {
      bar = new JProgressBar(99);
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;
    }
    harness.check(pass);
  }

  public void constructor4(TestHarness harness) 
  {
    harness.checkPoint("JProgressBar(int, int)");
    JProgressBar bar = new JProgressBar(12, 34);
    harness.check(bar.getOrientation(), JProgressBar.HORIZONTAL);
    harness.check(bar.getValue(), 12);
    harness.check(bar.getMinimum(), 12);
    harness.check(bar.getMaximum(), 34);
    harness.check(bar.getString(), "0%");
    harness.check(bar.isStringPainted(), false);
    
    boolean pass = false;
    try
    {
      bar = new JProgressBar(34, 12);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
  }

  public void constructor5(TestHarness harness) 
  {
    harness.checkPoint("JProgressBar(int, int, int)");
    JProgressBar bar = new JProgressBar(JProgressBar.VERTICAL, 12, 34);
    harness.check(bar.getOrientation(), JProgressBar.VERTICAL);
    harness.check(bar.getValue(), 12);
    harness.check(bar.getMinimum(), 12);
    harness.check(bar.getMaximum(), 34);
    harness.check(bar.getString(), "0%");
    harness.check(bar.isStringPainted(), false);
  }

}

