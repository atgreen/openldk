/* uidelegate.java -- some checks for the initialisation of the menu's UI
       delegate.
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

package gnu.testlet.javax.swing.JMenu;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JMenu;

/**
 * This test looks at the creation of the menu's UI delegate.  This test was
 * written to investigate a look and feel bug that occurs because the model is
 * null when the UI delegate is initialised.
 */
public class uidelegate implements Testlet {

  static class MyJMenu extends JMenu
  {
    public boolean initCalled;
    public boolean updateUICalledAfterInitCalled;
    public MyJMenu()
    {
      super();
    }
    public MyJMenu(Action action)
    {
      super(action);
    }
    public MyJMenu(String text)
    {
      super(text);
    }
    public MyJMenu(String text, boolean selected)
    {
      super(text, selected);
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
  }

  public void testConstructor1(TestHarness harness)
  {
    harness.checkPoint("()");
    MyJMenu b = new MyJMenu();
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
    MyJMenu b = new MyJMenu(action);
    harness.check(b.updateUICalledAfterInitCalled, true);
  }

  public void testConstructor3(TestHarness harness)
  {
    harness.checkPoint("(String)");
    MyJMenu b = new MyJMenu("ABC");
    harness.check(b.updateUICalledAfterInitCalled, true);
  }

  public void testConstructor4(TestHarness harness)
  {
    harness.checkPoint("(String, boolean)");
    MyJMenu b = new MyJMenu("ABC", false);
    harness.check(b.updateUICalledAfterInitCalled, true);
  }

}