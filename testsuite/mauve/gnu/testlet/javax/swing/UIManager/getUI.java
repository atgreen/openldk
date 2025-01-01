// Tags: JDK1.2
// Uses: MyLookAndFeel TestLabelUI

// Copyright (C) 2005 Roman Kennke <kennke@aicas.com>

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

package gnu.testlet.javax.swing.UIManager;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.JLabel;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;
import javax.swing.plaf.LabelUI;
import javax.swing.plaf.metal.MetalLookAndFeel;

/**
 * Some checks for the getUI() method in the 
 * {@link UIManager} class.
 */
public class getUI implements Testlet {

  class TestLabel extends JLabel
  {
    public void setUI(LabelUI ui)
    {
      // Overridden for test.
    }
  }

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    try
      {
        UIManager.setLookAndFeel(new MyLookAndFeel());
      }
    catch (Exception ex)
      {
        harness.fail(ex.getMessage());
      }

    TestLabel l = new TestLabel();
    UIManager.getUI(l);
    harness.check(TestLabelUI.installUICalled, false);
    
    // restore a sane look and feel
    try
    {
      UIManager.setLookAndFeel(new MetalLookAndFeel());
    }
    catch (UnsupportedLookAndFeelException e)
    {
      e.printStackTrace();
    }
    
  }
}
