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

import javax.swing.JSlider;

/**
 * Some checks for the setMinimum() method in the {@link JSlider} class.
 */
public class setMinimum implements Testlet {

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    JSlider slider = new JSlider(5, 10);
    harness.check(slider.getMinimum(), 5);
    harness.check(slider.getMaximum(), 10);
    harness.check(slider.getValue(), 7);
    
    slider.setMinimum(-5);
    harness.check(slider.getMinimum(), -5);
    harness.check(slider.getMaximum(), 10);
    harness.check(slider.getValue(), 7);
    
    slider.setMinimum(99);
    harness.check(slider.getMinimum(), 99);
    harness.check(slider.getMaximum(), 99);
    harness.check(slider.getValue(), 99);

    // confirm that this fires a ChangeEvent AND a PropertyChangeEvent
    MyChangeListener listener1 = new MyChangeListener();
    slider.addChangeListener(listener1);
    MyPropertyChangeListener listener2 = new MyPropertyChangeListener();
    slider.addPropertyChangeListener(listener2);
    slider.setMinimum(10);
    harness.check(listener1.event.getSource(), slider);
    harness.check(listener2.event.getSource(), slider);
    harness.check(listener2.event.getOldValue(), new Integer(99));
    harness.check(listener2.event.getNewValue(), new Integer(10));
    harness.check(listener2.event.getPropertyName(), "minimum");
    harness.check(listener2.event.getPropagationId(), null);
    
    // if the value isn't changed, there is no event
    listener1.event = null;
    listener2.event = null;
    slider.setMinimum(10);
    harness.check(listener1.event, null);
    harness.check(listener2.event, null);
  
  }

}
