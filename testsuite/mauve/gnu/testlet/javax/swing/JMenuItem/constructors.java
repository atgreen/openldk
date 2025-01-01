// Tags: JDK1.3

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

package gnu.testlet.javax.swing.JMenuItem;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import gnu.testlet.javax.swing.AbstractAction.MyAction;

import javax.swing.Action;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

/**
* Some tests for the constructors in the {@link JMenuItem} class.
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
    // insert other constructor tests later
  }

  private void testConstructor1(TestHarness harness)
  {
    harness.checkPoint("JMenuItem(Action)");
    
    // check for bug parade 4304129
    MyAction a = new MyAction();
    a.putValue(Action.NAME, "Action 1");
    a.putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke('A'));
    a.putValue(Action.MNEMONIC_KEY, new Integer(50));
    a.putValue(Action.ACTION_COMMAND_KEY, "ActionCommand");
    JMenuItem item = new JMenuItem(a);
    harness.check(item.getText(), "Action 1");
    harness.check(item.getAccelerator(), KeyStroke.getKeyStroke('A'));
    harness.check(item.getMnemonic(), 50);
    harness.check(item.getActionCommand(), "ActionCommand");
    
    // test null argument
    item = new JMenuItem((Action) null);
    harness.check(item.getText(), null);
    harness.check(item.getAccelerator(), null);
    harness.check(item.getMnemonic(), 0);
    harness.check(item.getActionCommand(), null);
  }

}
