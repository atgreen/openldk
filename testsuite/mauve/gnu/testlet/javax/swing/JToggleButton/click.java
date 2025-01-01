// Tags: JDK1.2

// Copyright (C) 2004 Audrius Meskauskas <audriusa@bluewin.ch>

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
 * The basic functions of the toggle button
 * that must also work when this component is not displayed.
 * @author Audrius Meskauskas (AudriusA@Bluewin.ch)
 */
public class click
  implements Testlet, ActionListener, ChangeListener
{
  static String UNASSIGNED = "<unassigned>";
  static String LABEL = "label";
  String command;
  boolean ceCalled;

  /** If the ActionListener has been called. */
  boolean alCalled;

  public void actionPerformed(ActionEvent e)
  {
    command = e.getActionCommand();
    alCalled = true;
  }

  public void stateChanged(ChangeEvent e)
  {
    ceCalled = true;
  }

  public void test(TestHarness harness)
  {
    JToggleButton b = createToggleButton(harness);

    testStateChanges(harness, b);
    testListeners(harness, b);
  }

  private void testListeners(TestHarness harness, JToggleButton b)
  {
    b.setSelected(false);
    b.addActionListener(this);
    b.addChangeListener(this);
    alCalled = false;
    b.doClick();
    harness.check(command, LABEL,
                  "Notifying action listener about the programmatic click"
                 );

    harness.check(ceCalled,
                  "Notifying change listener about the programmatic click"
                 );

    ceCalled = false;

    command = UNASSIGNED;

    b.setSelected(!b.isSelected());
    harness.check(ceCalled,
                  "Notifying change listener after the call of setSelected(..)"
                 );
    harness.check(command, UNASSIGNED,
                  "False message to the action listener after the call of "+
                  "setSelected(..)"
                 );

    ceCalled = false;
    command = UNASSIGNED;

    b.setSelected(b.isSelected());
    harness.check(!ceCalled,
                  "The change listener should only be notified about "+
                  "the CHANGES. Bug in 1.3, fixed in 1.4."
                 );

    command = UNASSIGNED;
    ceCalled = false;

    b.removeActionListener(this);
    b.removeChangeListener(this);
    b.doClick();
    harness.check(command, UNASSIGNED, "Removing action listener");
    harness.check(!ceCalled, "Removing change listener");
  }

  private void testStateChanges(TestHarness harness, JToggleButton b)
  {
    b.setSelected(false);
    b.doClick();
    harness.check(b.isSelected(), "state must alter to true after click");
    b.doClick();
    harness.check(!b.isSelected(), "state must alter to false after click");

    b.setSelected(true);
    harness.check(b.isSelected(), "manual state setting");
  }

  /**
   * Create the test object and also check its basic properties.
   */
  private JToggleButton createToggleButton(TestHarness harness)
  {
    JToggleButton b = new JToggleButton();
    b.setText(LABEL);
    harness.check(b.getText(), LABEL, LABEL);

    b.setToolTipText("tip");
    harness.check(b.getToolTipText(), "tip", "tooltip");

    harness.check(!b.isSelected(), "initial state");
    harness.check(b.isShowing(), false, "surely unvisible");
    harness.check(b.isOpaque(), true, "must be opaque by default");
    harness.check(b.getActionCommand(), LABEL, "getActionCommand");
    return b;
  }
}
