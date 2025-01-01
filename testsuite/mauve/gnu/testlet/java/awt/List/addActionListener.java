// addActionListener.java -- 

// Copyright (C) 2012 Pavel Tisnovsky <ptisnovs@redhat.com>

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
// the Free Software Foundation, Inc., 51 Franklin Street,
// Fifth Floor, Boston, MA 02110-1301 USA.

// Tags: GUI
// Uses: ../LocationTests

package gnu.testlet.java.awt.List;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import gnu.testlet.java.awt.LocationTests;

import java.awt.*;
import java.awt.event.*;

/**
  * Check if ActionListener could be registered for an AWT List.
  */
public class addActionListener
    implements Testlet
{

  /**
   * Runs the test using the specified harness. 
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)
  {
    List list = new List(10);
    list.setBackground(Color.blue);

    // array which will be filled by registered action listeners
    ActionListener[] actionListeners;

    // get all registered action listeners
    actionListeners = list.getActionListeners();
    harness.check(actionListeners.length, 0);

    // register new action listener
    list.addActionListener(
      new ActionListener() {
        public void actionPerformed(ActionEvent e)
        {
        // empty
        }

        @Override
        public String toString()
        {
          return "myActionListener";
        }
      }
    );

    // get all registered action listeners
    actionListeners = list.getActionListeners();
    harness.check(actionListeners.length, 1);

    // check if the proper listener is used
    harness.check(actionListeners[0].toString(), "myActionListener");
  }
}

