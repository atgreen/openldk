// Tags: JDK1.3
// Uses: MyAction

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
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.javax.swing.AbstractAction;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.ImageIcon;

/**
 * Some tests for the constructors in the {@link AbstractAction} class.
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
    testConstructor1(harness);
    testConstructor2(harness);
    testConstructor3(harness);
  }
  
  private void testConstructor1(TestHarness harness)
  {
    harness.checkPoint("AbstractAction()");
    MyAction a1 = new MyAction();
    harness.check(a1.getValue(Action.ACCELERATOR_KEY), null);
    harness.check(a1.getValue(Action.ACTION_COMMAND_KEY), null);
    harness.check(a1.getValue(Action.DEFAULT), null);
    harness.check(a1.getValue(Action.LONG_DESCRIPTION), null);
    harness.check(a1.getValue(Action.MNEMONIC_KEY), null);
    harness.check(a1.getValue(Action.NAME), null);
    harness.check(a1.getValue(Action.SHORT_DESCRIPTION), null);
    harness.check(a1.getValue(Action.SMALL_ICON), null);
    
    harness.check(a1.isEnabled());
  }

  private void testConstructor2(TestHarness harness)
  {
    harness.checkPoint("AbstractAction(String)");
    MyAction a1 = new MyAction("MyAction");
    harness.check(a1.getValue(Action.ACCELERATOR_KEY), null);
    harness.check(a1.getValue(Action.ACTION_COMMAND_KEY), null);
    harness.check(a1.getValue(Action.DEFAULT), null);
    harness.check(a1.getValue(Action.LONG_DESCRIPTION), null);
    harness.check(a1.getValue(Action.MNEMONIC_KEY), null);
    harness.check(a1.getValue(Action.NAME), "MyAction");
    harness.check(a1.getValue(Action.SHORT_DESCRIPTION), null);
    harness.check(a1.getValue(Action.SMALL_ICON), null);
    
    harness.check(a1.isEnabled());
    
    MyAction a2 = new MyAction(null);
    harness.check(a2.getValue(Action.NAME), null);
  }

  private void testConstructor3(TestHarness harness)
  {
    harness.checkPoint("AbstractAction(String, Icon)");
    Icon icon = new ImageIcon();
    MyAction a1 = new MyAction("MyAction", icon);
    harness.check(a1.getValue(Action.ACCELERATOR_KEY), null);
    harness.check(a1.getValue(Action.ACTION_COMMAND_KEY), null);
    harness.check(a1.getValue(Action.DEFAULT), null);
    harness.check(a1.getValue(Action.LONG_DESCRIPTION), null);
    harness.check(a1.getValue(Action.MNEMONIC_KEY), null);
    harness.check(a1.getValue(Action.NAME), "MyAction");
    harness.check(a1.getValue(Action.SHORT_DESCRIPTION), null);
    harness.check(a1.getValue(Action.SMALL_ICON), icon);
    
    harness.check(a1.isEnabled());
    
    MyAction a2 = new MyAction(null, icon);
    harness.check(a2.getValue(Action.NAME), null);
    harness.check(a2.getValue(Action.SMALL_ICON), icon);

    MyAction a3 = new MyAction("X", null);
    harness.check(a3.getValue(Action.NAME), "X");
    harness.check(a3.getValue(Action.SMALL_ICON), null);
  }

}
