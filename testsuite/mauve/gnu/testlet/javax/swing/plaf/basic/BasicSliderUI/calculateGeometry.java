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

import java.awt.Rectangle;

import javax.swing.JSlider;
import javax.swing.plaf.basic.BasicSliderUI;

/**
 * Some checks for the calculateGeometry() method in the {@link BasicSliderUI} 
 * class.
 */
public class calculateGeometry implements Testlet 
{
  public void test(TestHarness harness) 
  {
    JSlider slider = new JSlider();
    slider.setBounds(10, 20, 300, 40);
    MyBasicSliderUI ui = new MyBasicSliderUI(slider);
    ui.installUI(slider);
    ui.calculateGeometry();
    harness.check(ui.getContentRect(), new Rectangle(0, 0, 300, 40));
    harness.check(ui.getTrackBuffer(), 5);
    harness.check(ui.getFocusRect(), new Rectangle(0, 0, 300, 40));
    harness.check(ui.getTrackRect(), new Rectangle(5, 9, 290, 20));
    harness.check(ui.getThumbRect(), new Rectangle(145, 9, 11, 20));
    harness.check(ui.getTickRect(), new Rectangle(5, 28, 290, 0));
    
    slider.setPaintTicks(true);
    slider.setMajorTickSpacing(10);
    ui.calculateGeometry();
    harness.check(ui.getContentRect(), new Rectangle(0, 0, 300, 40));
    harness.check(ui.getTrackBuffer(), 5);
    harness.check(ui.getFocusRect(), new Rectangle(0, 0, 300, 40));
    harness.check(ui.getTrackRect(), new Rectangle(5, 5, 290, 20));
    harness.check(ui.getThumbRect(), new Rectangle(145, 5, 11, 20));
    harness.check(ui.getTickRect(), new Rectangle(5, 25, 290, 8));
  }
}
