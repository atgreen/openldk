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


/**
 * Tests bug 23918 (on setRepeats(false) only one event must be
 * fired.
 *
 * @author Audrius Meskauskas (AudriusA@Bluewin.ch)
 */
package gnu.testlet.javax.swing.Timer;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.Timer;

public class test_23918 implements Testlet, ActionListener
{
  int fired;
  public void test(TestHarness harness)
  {
    fired = 0;
    Timer timer = new Timer(50, this);
    timer.setRepeats(false);
    timer.start();
    try
      {
        Thread.sleep(400);
      }
    catch (InterruptedException ex)
    {
    }
    harness.check(fired, 1, "Must be fired exactly once.");
    harness.check(timer.isRunning(), false, "Must not be terminated");
  }

  public void actionPerformed(ActionEvent parm1)
  {
    fired++;
  }

}