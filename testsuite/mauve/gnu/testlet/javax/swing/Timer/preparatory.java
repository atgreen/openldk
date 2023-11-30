// Tags: JDK1.4

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

package gnu.testlet.javax.swing.Timer;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Timer;

/**
 * A limiting simple test that does not actually attempt to
 * start the timer, but checks other features of this class.
 * @author Audrius Meskauskas (AudriusA@Bluewin.ch)
 */
public class preparatory
  implements Testlet, ActionListener
{
  public static class otherListener
    implements ActionListener
  {
    public void actionPerformed(ActionEvent parm1)
    {
    }
  }

  static int DELAY = 7777;
  static int OTHER_DELAY = 5555;

  Timer object = new Timer(DELAY, null);

  /* Test the InitialDelay(). */
  public void test_InitialDelay(TestHarness harness)
  {
    Timer t = new Timer(DELAY, null);

    harness.check(t.getInitialDelay(), DELAY,
    "Initial delay must be be a default value from constructor, "+
     DELAY+", not "+object.getInitialDelay());

    t.setInitialDelay(1);
    harness.check(t.getInitialDelay(), 1,
     "Initial delay must be set to 1 by setInitialDelay(1)");

    t.setDelay(DELAY);
    harness.check(t.getInitialDelay(), 1, "If the initial delay is "+
    "explicitly set, setDelay() must not alter it.");
  }

  /* Test getDelay(). */
  public void test_Delay(TestHarness harness)
  {
    harness.check(object.getDelay(), DELAY, "getDelay()");
    object.setDelay(5);
    harness.check(object.getDelay(), 5);
  }

  /* Test getListeners(Class). */
  public void test_Listeners(TestHarness harness)
  {
    object.addActionListener(this);

    harness.checkPoint("getListeners");

    java.util.EventListener result[] =
      object.getListeners(ActionListener.class);
    harness.check(result.length == 1);
    harness.check(result [ 0 ] == this);

    result = object.getListeners(otherListener.class);
    harness.check(result.length == 0);

    object.removeActionListener(this);

    harness.check(object.getListeners(ActionListener.class).length, 0,
                  "Removing listener"
                 );
  }

  /* Test isCoalesce(). */
  public void test_Coalesce(TestHarness harness)
  {
    harness.checkPoint("isCoalesce");
    object.setCoalesce(true);
    harness.check(object.isCoalesce());
    object.setCoalesce(false);
    harness.check(!object.isCoalesce());
  }

  /* Test isRepeats(). */
  public void test_Repeats(TestHarness harness)
  {
    harness.checkPoint("Repeats");

    harness.check(object.isRepeats(), true, "isRepeats default value is true");

    object.setRepeats(true);
    harness.check(object.isRepeats());
    object.setRepeats(false);
    harness.check(!object.isRepeats());
  }

  /* Test isRunning(). */
  public void test_isRunning(TestHarness harness)
  {
    harness.check(!object.isRunning(), " should not be running ");
  }

  /* Test removeActionListener(java.awt.event.ActionListener). */
  public void test_add_removeActionListener(TestHarness harness)
  {
    harness.checkPoint("Adding/removing listeners");

    Timer t = new Timer(1, this);
    ActionListener other = new otherListener();
    t.addActionListener(other);

    harness.check(t.getActionListeners().length, 2, "must be 2 listeners");
    t.removeActionListener(this);

    harness.check(t.getActionListeners().length, 1, "must be 1 listener");

    t.removeActionListener(new otherListener());

    harness.check(t.getActionListeners().length, 1, "must still be 1 listener");

    t.removeActionListener(other);
    harness.check(t.getActionListeners().length, 0, "must be no listeners");
  }

  /* Test setLogTimers(boolean). */
  public void test_LogTimers(TestHarness harness)
  {
    object.setLogTimers(true);
    harness.check(object.getLogTimers(),"log timers");
    object.setLogTimers(false);
    harness.check(!object.getLogTimers());
  }

  public void test(TestHarness harness)
  {
    test_InitialDelay(harness);
    test_Delay(harness);
    test_Listeners(harness);
    test_Coalesce(harness);
    test_Repeats(harness);
    test_isRunning(harness);
    test_add_removeActionListener(harness);
    test_LogTimers(harness);
  }

  public void actionPerformed(ActionEvent parm1)
  {
  }
}
