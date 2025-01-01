// Tags: JDK1.4

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
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
// Boston, MA 02110-1301 USA.

package gnu.testlet.javax.swing.JComboBox;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.ComboBoxEditor;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.UIManager;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.metal.MetalComboBoxEditor;

/**
 * Some checks for the getEditor() method in the {@link JComboBox} class.
 */
public class getEditor 
  implements Testlet
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {   
    testDefault(harness);
    testPR30337(harness);
  }

  /**
   * Tests the default value.
   *
   * @param harness the test harness to use
   */
  private void testDefault(TestHarness harness)
  {
    // use a known look and feel
    try
      {
        UIManager.setLookAndFeel("javax.swing.plaf.metal.MetalLookAndFeel");
      }
    catch (Exception e)
      {
        harness.fail("Problem setting MetalLookAndFeel");
      }
    
    JComboBox c1 = new JComboBox(new Object[] {"A", "B", "C"});
    ComboBoxEditor editor = c1.getEditor();
    
    // check default value
    harness.check(editor instanceof MetalComboBoxEditor.UIResource);
    
    c1.setEditor(new MetalComboBoxEditor());
    harness.check(!(c1.getEditor() instanceof UIResource));

  }

  /**
   * A combo box that returns null for getEditor(). This is used in the
   * following test.
   */
  private class TestComboBox extends JComboBox
  {
    public ComboBoxEditor getEditor()
    {
      return null;
    }
  }

  /**
   * The Classpath PR30337 overrides getEditor() to return null and expects
   * the JComboBox to not throw an NPE.
   *
   * @param h the test harness
   */
  private void testPR30337(TestHarness h)
  {
    // Test with default (non-editable) combo box. No NPE is thrown.
    JComboBox cb = new TestComboBox();
    JFrame f = new JFrame();
    boolean pass = true;
    try
      {
        f.getContentPane().add(cb);
        f.setVisible(true);
      }
    catch (NullPointerException ex)
      {
        pass = false;
      }
    h.check(pass);

    // remove frame from the desktop
    f.dispose();

    // Test with editable combo box (NPE is thrown).
    cb = new TestComboBox();
    f = new JFrame();
    pass = false;
    try
      {
        cb.setEditable(true);
        f.getContentPane().add(cb);
        f.setVisible(true);
      }
    catch (NullPointerException ex)
      {
        pass = true;
      }
    h.check(pass);

    // remove frame from the desktop
    f.dispose();

  }
  
}

