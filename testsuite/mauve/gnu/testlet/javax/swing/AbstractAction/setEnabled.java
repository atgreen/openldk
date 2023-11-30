// Tags: JDK1.3
// Uses: MyAction MyPropertyChangeListener

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

  /**
   * Some tests for the setEnabled() method in the {@link AbstractAction} class.
   */
  public class setEnabled implements Testlet
  {

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    MyPropertyChangeListener listener = new MyPropertyChangeListener();
    MyAction a1 = new MyAction();
    a1.addPropertyChangeListener(listener);
    harness.check(a1.isEnabled());
    harness.check(!listener.getChange());  
    a1.setEnabled(true); // no change
    harness.check(!listener.getChange());
    a1.setEnabled(false);
    harness.check(listener.getChange());  
  }

}
