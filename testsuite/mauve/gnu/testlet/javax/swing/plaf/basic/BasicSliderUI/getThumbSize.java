// Tags: JDK1.2
// Uses: MyBasicSliderUI

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
 * Some checks for the getThumbSize method in the {@link BasicSliderUI} class.
 */
public class getThumbSize implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted). 
   */
  public void test(TestHarness harness)      
  {
    JSlider slider = new JSlider();
    slider.setUI(new MyBasicSliderUI(slider));
    MyBasicSliderUI b = (MyBasicSliderUI) slider.getUI();
    harness.check(b.getThumbSize(), new Dimension(11, 20));
    slider.setOrientation(JSlider.VERTICAL);
    harness.check(b.getThumbSize(), new Dimension(20, 11));    
  }
}

