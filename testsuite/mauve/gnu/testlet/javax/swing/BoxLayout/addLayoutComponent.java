// Tags: JDK1.2 

// Copyright (C) 2005 David Gilbert <david.gilbert@object-refinery.com>

// This file is part of Mauve.

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


package gnu.testlet.javax.swing.BoxLayout;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Component;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JPanel;

/**
 * Some checks for the addLayoutComponent methods in the {@link BoxLayout} 
 * class.
 */
public class addLayoutComponent implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness) 
  {
    test1(harness);
    test2(harness);
  }
  
  private void test1(TestHarness harness)
  {
    harness.checkPoint("(Component, Object)");
    // the addLayoutComponent() method is not used so nothing here should
    // fail
    BoxLayout layout = new BoxLayout(new JPanel(), BoxLayout.X_AXIS);
    layout.addLayoutComponent(new JPanel(), "XYZ");
    layout.addLayoutComponent((Component) null, (Object) null);
    harness.check(true);
  }
  
  private void test2(TestHarness harness)
  {
    harness.checkPoint("(String, Component)");
    // the addLayoutComponent() method is not used so nothing here should
    // fail
    BoxLayout layout = new BoxLayout(new JPanel(), BoxLayout.X_AXIS);
    layout.addLayoutComponent("Name", new JButton("Test"));
    layout.addLayoutComponent((String) null, (JComponent) null);
    harness.check(true);
  }

}

