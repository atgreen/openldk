/* taskException.java -- check if a Timer becomes cancelled if an
   exception is thrown.
   Copyright (C) 2004  Free Software Founddation, Inc.

This file is part of Mauve.

Mauve is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

Mauve is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Mauve; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

// Tags: JDK1.3


package gnu.testlet.java.util.Timer;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.util.Timer;
import java.util.TimerTask;

public class taskException implements Testlet
{
  private volatile boolean ran;

  public void test (TestHarness harness)
  {
    ran = false;
    harness.checkPoint ("Timer.schedule");
    Timer timer = new Timer (true);
    timer.schedule (new TimerTask()
      {
        public void run()
        {
          ran = true;
          throw new RuntimeException ("eat it!!!");
        }
      }, 10);
    try { Thread.sleep (50); } catch (InterruptedException ignore) {}
    harness.check (ran, "task was not run");
    try
      {
        timer.schedule (new TimerTask()
          {
            public void run()
            {
              ran = false;
            }
          }, 10);
        harness.check (false, "still able to schedule tasks");
      }
    catch (IllegalStateException ise)
      {
        harness.check (true);
      }
    harness.check (ran, "unschedulable task was run");
  }
}
