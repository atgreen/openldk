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

import javax.swing.BoundedRangeModel;
import javax.swing.DefaultBoundedRangeModel;
import javax.swing.JSlider;

/**
 * Some checks for the setModel() method in the {@link JSlider} class.
 */
public class setModel implements Testlet {

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    JSlider slider = new JSlider();
    BoundedRangeModel m1 = slider.getModel();
    MyChangeListener listener1= new MyChangeListener();
    slider.addChangeListener(listener1);
    MyPropertyChangeListener listener2 = new MyPropertyChangeListener();
    slider.addPropertyChangeListener(listener2);
    DefaultBoundedRangeModel m2 = new DefaultBoundedRangeModel(3, 0, 1, 6);
    slider.setModel(m2);
    harness.check(slider.getModel(), m2);
    harness.check(slider.getValue(), 3);
    harness.check(slider.getExtent(), 0);
    harness.check(slider.getMinimum(), 1);
    harness.check(slider.getMaximum(), 6);
    harness.check(listener1.event, null);
    harness.check(listener2.event.getSource(), slider);
    harness.check(listener2.event.getOldValue(), m1);
    harness.check(listener2.event.getNewValue(), m2);
    harness.check(listener2.event.getPropertyName(), "model");
    harness.check(listener2.event.getPropagationId(), null);
    
    // check that no event is generated if the model is not different
    listener2.event = null;
    slider.setModel(m2);
    harness.check(listener2.event, null);
    
    // try null argument
    boolean pass = false;
    try
    {
      slider.setModel(null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
  }

}
