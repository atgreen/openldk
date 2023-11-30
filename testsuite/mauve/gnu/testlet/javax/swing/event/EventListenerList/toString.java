// Tags: JDK1.2

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

package gnu.testlet.javax.swing.event.EventListenerList;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.util.EventListener;
import javax.swing.event.EventListenerList;


/**
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class toString
  implements Testlet
{
  private static class L1
    implements EventListener
  {
    public String toString()
    {
      return "l-one";
    }
  };

  private static class L2
    implements EventListener
  {
    public String toString()
    {
      return "l-two";
    }
  };

  public void test(TestHarness harness)
  {
    // Check #1.
    EventListenerList ell = new EventListenerList();
    ell.add(EventListener.class, new L1());
    ell.add(L2.class, new L2());
    harness.check(ell.toString(), "EventListenerList: 2 listeners:  "
                  + "type java.util.EventListener listener l-one "
                  + "type gnu.testlet.javax.swing.event.EventList"
                  + "enerList.toString$L2 listener l-two");
  }
}
