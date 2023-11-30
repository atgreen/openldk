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

import java.awt.Dimension;

import javax.swing.JSlider;
import javax.swing.plaf.basic.BasicSliderUI;

/**
 * Some checks for the getPreferredSize() method in the {@link BasicSliderUI} 
 * class.
 */
public class getPreferredSize implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted). 
   */
  public void test(TestHarness harness)      
  {
    JSlider slider1 = new JSlider(JSlider.HORIZONTAL);
    BasicSliderUI ui1 = new BasicSliderUI(slider1);
    slider1.setUI(ui1);
    Dimension d1 = ui1.getPreferredSize(slider1);
    harness.check(d1, new Dimension(200, 20));
    
    slider1.setPaintTicks(true);
    slider1.setMajorTickSpacing(10);
    d1 = ui1.getPreferredSize(slider1);
    harness.check(d1, new Dimension(200, 28));
    
    JSlider slider2 = new JSlider(JSlider.VERTICAL);
    BasicSliderUI ui2 = new BasicSliderUI(slider2);
    slider2.setUI(ui2);
    Dimension d2 = ui2.getPreferredSize(slider2);
    harness.check(d2, new Dimension(20, 200));
    
    slider2.setPaintTicks(true);
    slider2.setMajorTickSpacing(10);
    d2 = ui2.getPreferredSize(slider2);
    harness.check(d2, new Dimension(28, 200));
    
    // try null argument - the argument is ignored, probably the implementation
    // uses the internal slider reference
    d2 = ui2.getPreferredSize(null);
    harness.check(d2, new Dimension(28, 200));
  }
}

