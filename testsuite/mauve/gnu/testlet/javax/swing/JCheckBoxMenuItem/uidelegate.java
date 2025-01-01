/* uidelegate.java -- some checks for the initialisation of the menu item's 
       UI delegate.
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

package gnu.testlet.javax.swing.JCheckBoxMenuItem;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JToggleButton.ToggleButtonModel;
import javax.swing.plaf.metal.MetalIconFactory;

/**
 * This test looks at the creation of the menu item's UI delegate.  This test 
 * was written to investigate a look and feel bug that occurs because the model
 * is null when the UI delegate is initialised.
 */
public class uidelegate implements Testlet {

  static class MyJCheckBoxMenuItem extends JCheckBoxMenuItem
  {
    public boolean initCalled;
    public boolean updateUICalledAfterInitCalled;
    public MyJCheckBoxMenuItem()
    {
      super();
    }
    public MyJCheckBoxMenuItem(Action action)
    {
      super(action);
    }
    public MyJCheckBoxMenuItem(Icon icon)
    {
      super(icon);
    }
    public MyJCheckBoxMenuItem(String text)
    {
      super(text);
    }
    public MyJCheckBoxMenuItem(String text, boolean selected)
    {
      super(text, selected);
    }
    public MyJCheckBoxMenuItem(String text, Icon icon)
    {
      super(text, icon);
    }
    public MyJCheckBoxMenuItem(String text, Icon icon, boolean selected)
    {
      super(text, icon, selected);
    }
    
    public void init(String text, Icon icon)
    {
      initCalled = true;
      super.init(text, icon);
    }
    public void updateUI()
    {
      updateUICalledAfterInitCalled = initCalled;
      super.updateUI();
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
  }

  public void testConstructor1(TestHarness harness)
  {
    harness.checkPoint("()");
    MyJCheckBoxMenuItem b = new MyJCheckBoxMenuItem();
    harness.check(b.updateUICalledAfterInitCalled, true);
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
    MyJCheckBoxMenuItem b = new MyJCheckBoxMenuItem(action);
    harness.check(b.updateUICalledAfterInitCalled, true);
  }

  public void testConstructor3(TestHarness harness)
  {
    harness.checkPoint("(Icon)");
    MyJCheckBoxMenuItem b = new MyJCheckBoxMenuItem(
            MetalIconFactory.getFileChooserListViewIcon());
    harness.check(b.updateUICalledAfterInitCalled, true);
  }

  public void testConstructor4(TestHarness harness)
  {
    harness.checkPoint("(String)");
    MyJCheckBoxMenuItem b = new MyJCheckBoxMenuItem("ABC");
    harness.check(b.updateUICalledAfterInitCalled, true);
  }

  public void testConstructor5(TestHarness harness)
  {
    harness.checkPoint("(String, boolean)");
    MyJCheckBoxMenuItem b = new MyJCheckBoxMenuItem("ABC", false);
    harness.check(b.updateUICalledAfterInitCalled, true);
  }

  public void testConstructor6(TestHarness harness)
  {
    harness.checkPoint("(String, Icon)");
    MyJCheckBoxMenuItem b = new MyJCheckBoxMenuItem("ABC", 
            MetalIconFactory.getFileChooserDetailViewIcon());
    harness.check(b.updateUICalledAfterInitCalled, true);
  }

  public void testConstructor7(TestHarness harness)
  {
    harness.checkPoint("(String, Icon, boolean)");
    MyJCheckBoxMenuItem b = new MyJCheckBoxMenuItem("ABC", 
            MetalIconFactory.getFileChooserDetailViewIcon(), false);
    harness.check(b.updateUICalledAfterInitCalled, true);
  }
}