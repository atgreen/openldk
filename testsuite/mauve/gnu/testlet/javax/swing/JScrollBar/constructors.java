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
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
// Boston, MA 02110-1301 USA.

package gnu.testlet.javax.swing.JScrollBar;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.JProgressBar;
import javax.swing.JScrollBar;
import javax.swing.UIManager;
import javax.swing.plaf.metal.MetalLookAndFeel;

/**
 * Some checks for the constructors in the {@link JScrollBar} class.
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
  }

  public void constructor1(TestHarness harness) 
  {
    harness.checkPoint("JScrollBar()");

    // use a known look and feel
    try
    {
      UIManager.setLookAndFeel(new MetalLookAndFeel());
    }
    catch (Exception e)
    {
      harness.debug(e);
    }

    JScrollBar bar = new JScrollBar();
    harness.check(bar.getOrientation(), JScrollBar.VERTICAL);
    harness.check(bar.getValue(), 0);
    harness.check(bar.getMinimum(), 0);
    harness.check(bar.getMaximum(), 100);
    harness.check(bar.getClientProperty("JScrollBar.isFreeStanding"), null);
  }

  public void constructor2(TestHarness harness)  
  {
    harness.checkPoint("JScrollBar(int)");
    
    // use a known look and feel
    try
    {
      UIManager.setLookAndFeel(new MetalLookAndFeel());
    }
    catch (Exception e)
    {
      harness.debug(e);
    }

    JScrollBar bar = new JScrollBar(JScrollBar.HORIZONTAL);
    harness.check(bar.getOrientation(), JProgressBar.HORIZONTAL);
    harness.check(bar.getValue(), 0);
    harness.check(bar.getMinimum(), 0);
    harness.check(bar.getMaximum(), 100);
    harness.check(bar.getClientProperty("JScrollBar.isFreeStanding"), null);
    
    bar = new JScrollBar(JScrollBar.VERTICAL);
    harness.check(bar.getOrientation(), JScrollBar.VERTICAL);

    // check bad orientation
    boolean pass = false;
    try
    {
      bar = new JScrollBar(99);
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;
    }
    harness.check(pass);
  }

  public void constructor3(TestHarness harness) 
  {
    harness.checkPoint("JScrollBar(int, int, int, int, int)");
    JScrollBar bar = new JScrollBar(JScrollBar.VERTICAL, 50, 5, 0, 100);
    harness.check(bar.getOrientation(), JProgressBar.VERTICAL);
    harness.check(bar.getValue(), 50);
    harness.check(bar.getMinimum(), 0);
    harness.check(bar.getMaximum(), 100);
    harness.check(bar.getClientProperty("JScrollBar.isFreeStanding"), null);
    
    // check bad orientation
    boolean pass = false;
    try
    {
      bar = new JScrollBar(99, 50, 5, 0, 100);
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;
    }
    harness.check(pass);
    
    // TODO: check bad ranges
  }

}

