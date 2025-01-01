// Tags: JDK1.4

// Copyright (C) 2004 Sascha Brawer <brawer@dandelis.ch>

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

package gnu.testlet.java.util.logging.LogRecord;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.util.logging.Level;
import java.util.logging.LogRecord;

/**
 * @author <a href="mailto:brawer@dandelis.ch">Sascha Brawer</a>
 */
public class getThreadID
  implements Testlet
{
  public void test(TestHarness th)
  {
    LogRecord rec1 = new LogRecord(Level.CONFIG, "foo");
    LogRecord rec2 = new LogRecord(Level.CONFIG, "baz");
    LogRecord rec3 = createLogRecordInOtherThread();

    // Check #1.
    th.check(rec1.getThreadID() == rec2.getThreadID());

    // Check #2.
    th.check(rec2.getThreadID() != rec3.getThreadID());
  }


  /**
   * Returns a LogRecord that has been created in another thread.
   */
  private LogRecord createLogRecordInOtherThread()
  {
    class MyThread extends Thread
    {
      LogRecord  record;

      public void run()
      {	
	record = new LogRecord(Level.INFO, "foobar");
      }

      public LogRecord getRecord()
      {
	return record;
      }
    };

    MyThread theThread = new MyThread();
    try
    {
      theThread.start();
      theThread.join();
      return theThread.getRecord();
    }
    catch (InterruptedException _) {
      return null;
    }
  }
}
