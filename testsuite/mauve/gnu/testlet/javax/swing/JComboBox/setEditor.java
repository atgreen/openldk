// Tags: JDK1.4
// Uses: ../plaf/TestLookAndFeel

// Copyright (C) 2005, 2006 David Gilbert <david.gilbert@object-refinery.com>

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

package gnu.testlet.javax.swing.JComboBox;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import gnu.testlet.javax.swing.plaf.TestLookAndFeel;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.ComboBoxEditor;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.UIManager;
import javax.swing.plaf.basic.BasicComboBoxEditor;
import javax.swing.plaf.metal.MetalComboBoxEditor;
import javax.swing.plaf.metal.MetalLookAndFeel;

/**
 * Some checks for the setEditor() method in the {@link JComponent} class.
 */
public class setEditor 
  implements Testlet, PropertyChangeListener
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {   
    // use a known look and feel
    try
      {
        UIManager.setLookAndFeel(new MetalLookAndFeel());
      }
    catch (Exception e)
      {
        harness.fail("Problem setting MetalLookAndFeel");
      }
    
    JComboBox c1 = new JComboBox(new Object[] {"A", "B", "C"});
    c1.addPropertyChangeListener(this);
    
    ComboBoxEditor editor = new MetalComboBoxEditor();
    c1.setEditor(editor);
    harness.check(c1.getEditor(), editor);
    harness.check(event.getPropertyName(), "editor");
    harness.check(event.getNewValue(), editor);
    
    // set a new look and feel and see if the editor (which doesn't implement
    // UIResource) gets replaced
    try
      {
        UIManager.setLookAndFeel(new TestLookAndFeel());
      }
    catch (Exception e)
      {
        harness.fail("Problem setting TestLookAndFeel");
      }
    c1.updateUI();
    JComboBox c2 = new JComboBox();
    harness.check(c1.getEditor(), editor);
    harness.check(c2.getEditor() instanceof BasicComboBoxEditor.UIResource);
    
    // restore MetalLookAndFeel so as not to interfere with other tests
    try
      {
        UIManager.setLookAndFeel(new MetalLookAndFeel());
      }
    catch (Exception e)
      {
        harness.fail("Problem restoring MetalLookAndFeel");
      }
    
    // try a null setting - no exceptions are thrown
    c1.setEditor(null);
    harness.check(c1.getEditor(), null);
  }
  
  private PropertyChangeEvent event;
  
  public void propertyChange(PropertyChangeEvent e)
  {
    event = e;
  }
  
}

