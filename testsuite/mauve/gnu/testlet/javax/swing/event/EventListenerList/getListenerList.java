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
public class getListenerList
  implements Testlet
{
  private static class L
    implements EventListener
  {
  };


  public void test(TestHarness harness)
  {
    EventListenerList ell = new EventListenerList();
    EventListener l1 = new L();
    L l2 = new L();
    Object[] list;

    // Check #1.
    list = ell.getListenerList();
    harness.check(list.length, 0);

    // Check #2.
    ell.add(EventListener.class, l1);
    ell.add(L.class, l2);
    list = ell.getListenerList();
    harness.check(list.length, 4);

    // Check #3.
    harness.check(list[0] == EventListener.class);

    // Check #4.
    harness.check(list[1] == l1);

    // Check #5.
    harness.check(list[2] == L.class);

    // Check #6.
    harness.check(list[3] == l2);

    // Check #7: Same array returned upon each invocation.
    harness.check(ell.getListenerList() == list);

    // Check #8.
    ell.remove(EventListener.class, l1);
    list = ell.getListenerList();
    harness.check(list.length, 2);

    // Check #9.
    harness.check(list[0] == L.class);

    // Check #10.
    harness.check(list[1] == l2);
  }
}
