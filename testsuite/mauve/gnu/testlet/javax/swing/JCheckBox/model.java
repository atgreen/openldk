/* model.java -- some checks for the initialisation of the button's model.
   Copyright (C) 2006 David Gilbert <david.gilbert@object-refinery.com>
This file is part of Mauve.

Mauve is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Mauve is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Mauve; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

*/

// Tags: JDK1.5

package gnu.testlet.javax.swing.JCheckBox;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JCheckBox;
import javax.swing.JToggleButton.ToggleButtonModel;
import javax.swing.plaf.metal.MetalIconFactory;

/**
 * This test looks at the creation of the menu's model.  This test was
 * written to investigate a look and feel bug that occurs because the model is
 * null when the UI delegate is initialised.
 */
public class model implements Testlet {

  static class MyJCheckBox extends JCheckBox
  {
    public MyJCheckBox()
    {
      super();
    }
    public MyJCheckBox(Action action)
    {
      super(action);
    }
    public MyJCheckBox(Icon icon)
    {
      super(icon);
    }
    public MyJCheckBox(Icon icon, boolean selected)
    {
      super(icon, selected);
    }
    public MyJCheckBox(String text)
    {
      super(text);
    }
    public MyJCheckBox(String text, boolean selected)
    {
      super(text, selected);
    }
    public MyJCheckBox(String text, Icon icon)
    {
      super(text, icon);
    }
    public MyJCheckBox(String text, Icon icon, boolean selected)
    {
      super(text, icon, selected);
    }
    
    public void init(String text, Icon icon)
    {
      // don't call super.init(), because we want to check what happens in
      // the constructor only...
    }
  }
  
  public void test(TestHarness harness)
  {
    testConstructor1(harness);
    testConstructor2(harness);
    testConstructor3(harness);
    testConstructor4(harness);
    testConstructor5(harness);
    testConstructor6(harness);
    testConstructor7(harness);
    testConstructor8(harness);
  }

  public void testConstructor1(TestHarness harness)
  {
    harness.checkPoint("()");
    MyJCheckBox b = new MyJCheckBox();
    harness.check(b.getModel().getClass(), ToggleButtonModel.class);
  }
  
  public void testConstructor2(TestHarness harness)
  {
    harness.checkPoint("(Action)");
    Action action = new AbstractAction() {
      public void actionPerformed(ActionEvent e) 
      {
        // do nothing
      }
    };
    MyJCheckBox b = new MyJCheckBox(action);
    harness.check(b.getModel().getClass(), ToggleButtonModel.class);
  }

  public void testConstructor3(TestHarness harness)
  {
    harness.checkPoint("(Icon)");
    MyJCheckBox b = new MyJCheckBox(
            MetalIconFactory.getFileChooserListViewIcon());
    harness.check(b.getModel().getClass(), ToggleButtonModel.class);
  }

  public void testConstructor4(TestHarness harness)
  {
    harness.checkPoint("(Icon, boolean)");
    MyJCheckBox b = new MyJCheckBox(
            MetalIconFactory.getFileChooserListViewIcon(), false);
    harness.check(b.getModel().getClass(), ToggleButtonModel.class);
  }
  
  public void testConstructor5(TestHarness harness)
  {
    harness.checkPoint("(String)");
    MyJCheckBox b = new MyJCheckBox("ABC");
    harness.check(b.getModel().getClass(), ToggleButtonModel.class);
  }

  public void testConstructor6(TestHarness harness)
  {
    harness.checkPoint("(String, boolean)");
    MyJCheckBox b = new MyJCheckBox("ABC", false);
    harness.check(b.getModel().getClass(), ToggleButtonModel.class);
  }
  
  public void testConstructor7(TestHarness harness)
  {
    harness.checkPoint("(String, Icon)");
    MyJCheckBox b = new MyJCheckBox("ABC", 
            MetalIconFactory.getFileChooserDetailViewIcon());
    harness.check(b.getModel().getClass(), ToggleButtonModel.class);
  }

  public void testConstructor8(TestHarness harness)
  {
    harness.checkPoint("(String, Icon, boolean)");
    MyJCheckBox b = new MyJCheckBox("ABC", 
            MetalIconFactory.getFileChooserDetailViewIcon(), false);
    harness.check(b.getModel().getClass(), ToggleButtonModel.class);
  }
}