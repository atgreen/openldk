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
public class setSequenceNumber
  implements Testlet
{
  public void test(TestHarness th)
  {
    LogRecord rec1 = new LogRecord(Level.CONFIG, "msg");
    LogRecord rec2 = new LogRecord(Level.CONFIG, "msg2");
    long s1, s2;

    s1 = rec1.getSequenceNumber();
    s2 = rec2.getSequenceNumber();
    
    /* Check #1.
     *
     * It could happen that rec1 has a sequence number of
     * Long.MAX_VALUE, or that rec1's sequence number is close to
     * Long.MAX_VALUE and some background threads have created a few
     * LogRecords between the creation of rec1 and rec2, so rec2's
     * sequence number is equal to or slightly greater than
     * Long.MIN_VALUE. In these cases, s2 would not be greater than
     * s1, although the implementation of java.util.logging.LogRecord
     * was entirely correct.
     *
     * While this event is extremely unlikely, it is not entirely
     * impossible, so we can perform the subsequent check only if
     * there was no arithmetic overflow.
     */
    if ((s1 >= 0) == (s2 >= 0))
      th.check(s2 > s1);
    else
      th.check(true);

    // Check #2.
    rec2.setSequenceNumber(42);
    th.check(rec2.getSequenceNumber() == 42);

    // Check #3.
    rec2.setSequenceNumber(s2);    
    th.check(rec2.getSequenceNumber() == s2);
  }
}
