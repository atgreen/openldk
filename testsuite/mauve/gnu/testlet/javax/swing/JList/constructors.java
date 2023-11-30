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
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
// Boston, MA 02110-1301 USA.

package gnu.testlet.javax.swing.JList;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.Vector;

import javax.swing.JList;
import javax.swing.ListModel;
import javax.swing.ListSelectionModel;
import javax.swing.UIManager;
import javax.swing.plaf.metal.MetalLookAndFeel;

/**
 * Some checks for the constructors in the 
 * {@link JList} class.
 */
public class constructors implements Testlet
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)       
  {
    try
    {
      UIManager.setLookAndFeel("javax.swing.plaf.metal.MetalLookAndFeel");
    }
    catch (Exception e)
    {
      e.printStackTrace();
    }
    testConstructor1(harness);
    testConstructor2(harness);
    testConstructor3(harness);
    testConstructor4(harness);
  }

  public void testConstructor1(TestHarness harness)
  {
    harness.checkPoint("()");
    JList list = new JList();
    harness.check(list.getFont(), MetalLookAndFeel.getControlTextFont());
    harness.check(list.getForeground(), MetalLookAndFeel.getBlack());
    harness.check(list.getBackground(), 
        MetalLookAndFeel.getWindowBackground());
    harness.check(list.getDragEnabled(), false);
    harness.check(list.getSelectionMode(), 
            ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
  }

  public void testConstructor2(TestHarness harness)
  {
    harness.checkPoint("(ListModel)");
    JList list = new JList();
    harness.check(list.getFont(), MetalLookAndFeel.getControlTextFont());
    harness.check(list.getForeground(), MetalLookAndFeel.getBlack());
    harness.check(list.getBackground(), 
        MetalLookAndFeel.getWindowBackground());      
    harness.check(list.getDragEnabled(), false);
    harness.check(list.getSelectionMode(), 
            ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
    
    // check that null argument triggers a IllegalArgumentException
    boolean pass = false;
    try
    {
      /*JList list2 =*/ new JList((ListModel) null);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
  }

  public void testConstructor3(TestHarness harness)
  {
    harness.checkPoint("(Object[])");
    JList list = new JList();
    harness.check(list.getFont(), MetalLookAndFeel.getControlTextFont());
    harness.check(list.getForeground(), MetalLookAndFeel.getBlack());
    harness.check(list.getBackground(), 
        MetalLookAndFeel.getWindowBackground());      
    harness.check(list.getDragEnabled(), false);
    harness.check(list.getSelectionMode(), 
            ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
    
    // check that null argument is OK
    JList list2 = new JList((Object[]) null);
    harness.check(list2.getModel() != null);
  }

  public void testConstructor4(TestHarness harness)
  {
    harness.checkPoint("(Vector)");
    JList list = new JList();
    harness.check(list.getFont(), MetalLookAndFeel.getControlTextFont());
    harness.check(list.getForeground(), MetalLookAndFeel.getBlack());
    harness.check(list.getBackground(), 
        MetalLookAndFeel.getWindowBackground());      
    harness.check(list.getDragEnabled(), false);
    harness.check(list.getSelectionMode(), 
            ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
    
    // check that null argument is OK
    JList list2 = new JList((Vector) null);
    harness.check(list2.getModel() != null);
  }

}
