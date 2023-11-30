// Tags: JDK1.2
// Uses: MyChangeListener MyPropertyChangeListener

// Copyright (C) 2005 David Gilbert <david.gilbert@object-refinery.com>
//               2006 Red Hat

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
import java.util.Hashtable;

import javax.swing.JLabel;
import javax.swing.JSlider;

/**
 * Some checks for the setLabelTable() method in the {@link JSlider} class.
 */
public class setLabelTable implements Testlet {

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    JSlider slider = new JSlider(5, 10);
    Hashtable labels = new Hashtable();
    labels.put(new Integer(1), new JLabel("Label 1"));
    slider.setLabelTable(labels);
    harness.check(slider.getLabelTable(), labels);
    
    // confirm that this fires a PropertyChangeEvent and no ChangeEvent
    MyChangeListener listener1 = new MyChangeListener();
    slider.addChangeListener(listener1);
    MyPropertyChangeListener listener2 = new MyPropertyChangeListener();
    slider.addPropertyChangeListener(listener2);
    Hashtable labels2 = new Hashtable();
    labels2.put(new Integer(2), new JLabel("Label 2"));
    slider.setLabelTable(labels2);
    harness.check(listener1.event, null);
    harness.check(listener2.event.getSource(), slider);
    harness.check(listener2.event.getOldValue(), labels);
    harness.check(listener2.event.getNewValue(), labels2);
    harness.check(listener2.event.getPropertyName(), "labelTable");
    harness.check(listener2.event.getPropagationId(), null);
    listener2.event = null;
    
    // check that there is no event if the property is not changed
    slider.setLabelTable(labels2);
    harness.check(listener2.event, null);
    
    // try null table
    slider.setLabelTable(null);
    harness.check(slider.getLabelTable(), null);
    
    // This next test shows that it is not necessary to cast 
    // "list.nextElement()" to type JLabel.
    // NOTE:  With the casting, a ClassCastException was thrown.
    boolean pass = false;
    Dictionary labelTable = new Hashtable();
    labelTable.put("a", "b");
    try
      {
        slider.setLabelTable(labelTable);
        pass = true;
      }
    catch (ClassCastException e)
      {
        // Do nothing.
      }
    harness.check(pass);
  }

}
