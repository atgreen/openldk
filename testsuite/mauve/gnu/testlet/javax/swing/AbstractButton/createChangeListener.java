// Tags: JDK1.2
// Uses: TestButton

// Copyright (C) 2005 Roman Kennke <roman@kennke.org>

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
// Boston, MA 02111-1307, USA.

package gnu.testlet.javax.swing.AbstractButton;

import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Checks if the ChangeListener returned by this method is supposed to
 * trigger repaint and/or revalidate.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class createChangeListener implements Testlet
{

  /**
   * Starts the test run.
   *
   * @param harness the test harness to use
   */
  public void test(TestHarness harness)
  {
    TestButton b = new TestButton();
    ChangeListener l = b.createChangeListener();
    b.repaintCalled = false;
    b.revalidateCalled = false;
    l.stateChanged(new ChangeEvent(b.getModel()));
    harness.check(b.repaintCalled, true);
    harness.check(b.revalidateCalled, false);
  }

}
