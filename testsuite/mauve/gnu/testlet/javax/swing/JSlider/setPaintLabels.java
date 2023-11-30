// Tags: JDK1.2
// Uses: MyChangeListener MyPropertyChangeListener

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

import java.util.Dictionary;

import javax.swing.JSlider;

/**
 * Some checks for the setPaintLabels() method in the {@link JSlider} 
 * class.
 */
public class setPaintLabels implements Testlet {

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)  
  {
    testGeneral(harness);
    testLabelTable(harness);
  }
  
  public void testGeneral(TestHarness harness)      
  {
    JSlider slider = new JSlider(50, 100);
    MyChangeListener listener1 = new MyChangeListener();
    slider.addChangeListener(listener1);
    MyPropertyChangeListener listener2 = new MyPropertyChangeListener();
    slider.addPropertyChangeListener(listener2);
    slider.setMajorTickSpacing(10);
    slider.setPaintLabels(true);
    harness.check(slider.getPaintLabels(), true);
    harness.check(listener1.event, null);
    harness.check(listener2.event.getSource(), slider);
    harness.check(listener2.event.getOldValue(), Boolean.FALSE);
    harness.check(listener2.event.getNewValue(), Boolean.TRUE);
    harness.check(listener2.event.getPropertyName(), "paintLabels");
    harness.check(listener2.event.getPropagationId(), null);
    
    // check that no event is generated if the flag is not different
    listener2.event = null;
    slider.setPaintLabels(true);
    harness.check(listener2.event, null);
    
  }
  
  public void testLabelTable(TestHarness harness)      
  {
    JSlider slider = new JSlider(50, 100);
    slider.setMajorTickSpacing(10);
    harness.check(slider.getLabelTable(), null);
    slider.setPaintLabels(true);
    harness.check(slider.getPaintLabels(), true);
    Dictionary labels = slider.getLabelTable();
    harness.check(labels.size(), 6);    
  }

}
