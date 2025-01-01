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
 * Some checks for the setMajorTickSpacing() method in the {@link JSlider} 
 * class.
 */
public class setMajorTickSpacing implements Testlet {

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    JSlider slider = new JSlider(50, 100);
    slider.setMajorTickSpacing(10);
    harness.check(slider.getMajorTickSpacing(), 10);
    harness.check(slider.getLabelTable(), null);
    
    // confirm that this fires a PropertyChangeEvent and no ChangeEvent
    MyChangeListener listener1 = new MyChangeListener();
    slider.addChangeListener(listener1);
    MyPropertyChangeListener listener2 = new MyPropertyChangeListener();
    slider.addPropertyChangeListener(listener2);
    slider.setMajorTickSpacing(11);
    harness.check(listener1.event, null);
    harness.check(listener2.event.getSource(), slider);
    harness.check(listener2.event.getOldValue(), new Integer(10));
    harness.check(listener2.event.getNewValue(), new Integer(11));
    harness.check(listener2.event.getPropertyName(), "majorTickSpacing");
    harness.check(listener2.event.getPropagationId(), null);

    // check that no event occurs if the property value is not different
    listener2.event = null;
    slider.setMajorTickSpacing(11);
    harness.check(listener2.event, null);
    
    
    slider.setMajorTickSpacing(10);
    slider.setPaintLabels(true);
    harness.check(slider.getLabelTable().size(), 6);
    slider.setMajorTickSpacing(25);
    harness.check(slider.getLabelTable().size(), 6);  // not updated right away
  }

}
