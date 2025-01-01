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

package gnu.testlet.javax.swing.plaf.basic.BasicSliderUI;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.JSlider;
import javax.swing.plaf.basic.BasicSliderUI;

/**
 * Some checks for the valueForYPosition() method in the {@link BasicSliderUI} 
 * class.
 */
public class valueForYPosition implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted). 
   */
  public void test(TestHarness harness)      
  {
    JSlider slider1 = new JSlider(JSlider.VERTICAL);
    slider1.setBounds(10, 20, 40, 400);
    BasicSliderUI ui1 = new BasicSliderUI(slider1);
    slider1.setUI(ui1);
    harness.check(ui1.valueForYPosition(0), 100);
    harness.check(ui1.valueForYPosition(200), 50);
    harness.check(ui1.valueForYPosition(400), 0);
    harness.check(ui1.valueForYPosition(-50), 100);
    harness.check(ui1.valueForYPosition(450), 0);
  }
}

