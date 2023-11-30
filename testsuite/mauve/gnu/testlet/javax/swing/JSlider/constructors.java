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
 
package gnu.testlet.javax.swing.JSlider;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.BoundedRangeModel;
import javax.swing.DefaultBoundedRangeModel;
import javax.swing.JSlider;

/**
 * Some checks for the constructors in the {@link JSlider} class.
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
    constructor6(harness);
  }
  
  public void constructor1(TestHarness harness) 
  {
    harness.checkPoint("JSlider()");
    JSlider slider = new JSlider();
    harness.check(slider.getOrientation(), JSlider.HORIZONTAL);
    harness.check(slider.getMinimum(), 0);
    harness.check(slider.getMaximum(), 100);
    harness.check(slider.getMajorTickSpacing(), 0);
    harness.check(slider.getMinorTickSpacing(), 0);
    harness.check(slider.getPaintLabels(), false);
    harness.check(slider.getPaintTicks(), false);
    harness.check(slider.getSnapToTicks(), false);
    harness.check(slider.getValue(), 50);
    harness.check(slider.getExtent(), 0);
  }

  public void constructor2(TestHarness harness) 
  {
    harness.checkPoint("JSlider(BoundedRangeModel)");
    BoundedRangeModel m = new DefaultBoundedRangeModel(5, 0, 2, 9);
    JSlider slider = new JSlider(m);
    harness.check(slider.getMinimum(), 2);
    harness.check(slider.getMaximum(), 9);
    harness.check(slider.getMajorTickSpacing(), 0);
    harness.check(slider.getMinorTickSpacing(), 0);
    harness.check(slider.getPaintLabels(), false);
    harness.check(slider.getPaintTicks(), false);
    harness.check(slider.getSnapToTicks(), false);
    harness.check(slider.getValue(), 5);
    harness.check(slider.getExtent(), 0);
    
    // try null model
    boolean pass = false;
    try
    {
      /*JSlider slider =*/ new JSlider(null);
    }
    catch (NullPointerException e) 
    {
      pass = true;
    }
    harness.check(pass);
  }

  public void constructor3(TestHarness harness) 
  {
    harness.checkPoint("JSlider(int)");
    JSlider slider = new JSlider(JSlider.HORIZONTAL);
    harness.check(slider.getOrientation(), JSlider.HORIZONTAL);
    harness.check(slider.getMinimum(), 0);
    harness.check(slider.getMaximum(), 100);
    harness.check(slider.getMajorTickSpacing(), 0);
    harness.check(slider.getMinorTickSpacing(), 0);
    harness.check(slider.getPaintLabels(), false);
    harness.check(slider.getPaintTicks(), false);
    harness.check(slider.getSnapToTicks(), false);
    harness.check(slider.getValue(), 50);
    harness.check(slider.getExtent(), 0);
    
    slider = new JSlider(JSlider.VERTICAL);
    harness.check(slider.getOrientation(), JSlider.VERTICAL);
    harness.check(slider.getMinimum(), 0);
    harness.check(slider.getMaximum(), 100);
    harness.check(slider.getMajorTickSpacing(), 0);
    harness.check(slider.getMinorTickSpacing(), 0);
    harness.check(slider.getPaintLabels(), false);
    harness.check(slider.getPaintTicks(), false);
    harness.check(slider.getSnapToTicks(), false);
    harness.check(slider.getValue(), 50);
    harness.check(slider.getExtent(), 0);
    
    // try unrecognised constant
    boolean pass = false;
    try
    {
      slider = new JSlider(99);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
  }

  public void constructor4(TestHarness harness) 
  {
    harness.checkPoint("JSlider(int, int)");
    JSlider slider = new JSlider(23, 34);
    harness.check(slider.getOrientation(), JSlider.HORIZONTAL);
    harness.check(slider.getMinimum(), 23);
    harness.check(slider.getMaximum(), 34);
    harness.check(slider.getMajorTickSpacing(), 0);
    harness.check(slider.getMinorTickSpacing(), 0);
    harness.check(slider.getPaintLabels(), false);
    harness.check(slider.getPaintTicks(), false);
    harness.check(slider.getSnapToTicks(), false);
    harness.check(slider.getValue(), 28);
    harness.check(slider.getExtent(), 0);
    
    // what happens if min > max
    boolean pass = false;
    try
    {
      slider = new JSlider(20, 10);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
  }

  public void constructor5(TestHarness harness) 
  {
    harness.checkPoint("JSlider(int, int, int)");
    JSlider slider = new JSlider(23, 34, 33);
    harness.check(slider.getOrientation(), JSlider.HORIZONTAL);
    harness.check(slider.getMinimum(), 23);
    harness.check(slider.getMaximum(), 34);
    harness.check(slider.getMajorTickSpacing(), 0);
    harness.check(slider.getMinorTickSpacing(), 0);
    harness.check(slider.getPaintLabels(), false);
    harness.check(slider.getPaintTicks(), false);
    harness.check(slider.getSnapToTicks(), false);
    harness.check(slider.getValue(), 33);
    harness.check(slider.getExtent(), 0);
    
    // what happens if value > max
    boolean pass = false;
    try
    {
      slider = new JSlider(1, 2, 3);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);

    // what happens if value < min
    pass = false;
    try
    {
      slider = new JSlider(1, 2, 0);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);

    // what happens if min > max
    pass = false;
    try
    {
      slider = new JSlider(20, 10, 15);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
  }

  public void constructor6(TestHarness harness) 
  {
    harness.checkPoint("JSlider(int, int, int, int)");
    JSlider slider = new JSlider(JSlider.VERTICAL, 23, 34, 33);
    harness.check(slider.getOrientation(), JSlider.VERTICAL);
    harness.check(slider.getMinimum(), 23);
    harness.check(slider.getMaximum(), 34);
    harness.check(slider.getMajorTickSpacing(), 0);
    harness.check(slider.getMinorTickSpacing(), 0);
    harness.check(slider.getPaintLabels(), false);
    harness.check(slider.getPaintTicks(), false);
    harness.check(slider.getSnapToTicks(), false);
    harness.check(slider.getValue(), 33);
    harness.check(slider.getExtent(), 0);

    // what happens if value > max
    boolean pass = false;
    try
    {
      slider = new JSlider(JSlider.VERTICAL, 1, 2, 3);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);

    // what happens if value < min
    pass = false;
    try
    {
      slider = new JSlider(JSlider.VERTICAL, 1, 2, 0);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);

    // what happens if min > max
    pass = false;
    try
    {
      slider = new JSlider(JSlider.VERTICAL, 20, 10, 15);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
    
  }

}

