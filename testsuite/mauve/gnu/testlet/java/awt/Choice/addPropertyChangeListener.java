// addPropertyChangeListener.java -- 

// Copyright (C) 2011 Pavel Tisnovsky <ptisnovs@redhat.com>

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

package gnu.testlet.java.awt.Choice;

import java.awt.Color;
import java.awt.Choice;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Check if {@link PropertyChangeListener} could be registered for an AWT {@link Choice}.
 */
public class addPropertyChangeListener
implements Testlet
{
  private static final String PROPERTY_CHANGE_LISTENER_NAME = "myProperlyChangeListener";

  /**
   * Runs the test using the specified harness. 
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)
  {
    Choice choice = new Choice();
    choice.setBackground(Color.blue);

    // array which will be filled by registered listeners
    PropertyChangeListener[] properlyChangeListeners;

    // get all registered listeners
    properlyChangeListeners = choice.getPropertyChangeListeners();
    harness.check(properlyChangeListeners.length, 0);

    // register new listener
    choice.addPropertyChangeListener(new PropertyChangeListener() {
      public void propertyChange(PropertyChangeEvent evt) {
        // empty block
      }

      @Override
      public String toString() {
        return PROPERTY_CHANGE_LISTENER_NAME;
      }
    });

    // get all registered listeners
    properlyChangeListeners = choice.getPropertyChangeListeners();
    harness.check(properlyChangeListeners.length, 1);

    // check if the proper listener is used
    harness.check(properlyChangeListeners[0].toString(), PROPERTY_CHANGE_LISTENER_NAME);
  }

}
