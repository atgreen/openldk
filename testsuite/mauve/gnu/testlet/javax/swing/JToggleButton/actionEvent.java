// Tags: JDK1.2

// Copyright (C) 2005 Roman Kennke (roman@kennke.org)

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

package gnu.testlet.javax.swing.JToggleButton;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JToggleButton;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

/**
 * Tests if a registered ActionListener is notfied correctly.
 *
 * @author Roman Kennke
 */
public class actionEvent
  implements Testlet, ActionListener
{
  /** If the ActionListener was notified. */
  boolean alNotified = false;

  public void actionPerformed(ActionEvent e)
  {
    alNotified = true;
  }

  public void test(TestHarness harness)
  {
    JToggleButton b = new JToggleButton();
    b.addActionListener(this);

    testProgrammaticChanges(harness, b);
    testUserChanges(harness, b);
  }

  /**
   * Tests if the ActionListener is correctly notfified of changes
   * triggered by setSelected().
   */
  void testProgrammaticChanges(TestHarness harness, JToggleButton b)
  {
    b.setSelected(false);

    alNotified = false;
    b.setSelected(false);
    harness.check(alNotified, false,
                  "Should not be notified on programmatic state change");

    alNotified = false;
    b.setSelected(true);
    harness.check(alNotified, false,
                  "Should not be notified on programmatic state change");

    alNotified = false;
    b.setSelected(true);
    harness.check(alNotified, false,
                  "Should not be notified on programmatic state change");

    alNotified = false;
    b.setSelected(false);
    harness.check(alNotified, false,
                  "Should not be notified on programmatic state change");

  }

  /**
   * Tests if the ActionListener is correctly notfified of changes
   * triggered by user clicks.
   */
  void testUserChanges(TestHarness harness, JToggleButton b)
  {
    b.setSelected(false);

    alNotified = false;
    b.doClick();
    harness.check(alNotified, true,
                  "Should be notified on user click");

    alNotified = false;
    b.doClick();
    harness.check(alNotified, true,
                  "Should be notified on user click");
  }
}
