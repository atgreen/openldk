// Tags: JDK1.3

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
 * These tests pass with the Sun JDK 1.4.1_01 on GNU/Linux IA-32.
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class getListeners
  implements Testlet
{
  private static class L1
    implements EventListener
  {
  };

  private static class L2
    implements EventListener
  {
  };

  private static class L3
    extends L2
  {
  };

  private static class L4
    implements EventListener
  {
  };

  public void test(TestHarness harness)
  {
    EventListenerList ell = new EventListenerList();
    L1 l1a = new L1();
    L1 l1b = new L1();
    L2 l2 = new L2();
    L3 l3 = new L3();
    EventListener[] list;
    Throwable caught;

    ell.add(EventListener.class, l1a);
    ell.add(L2.class, l2);
    ell.add(L3.class, l3);
    ell.add(L1.class, l1b);
    list = ell.getListeners(L1.class);
    
    harness.check(list.length, 1); // #1.
    harness.check(list[0] == l1b); // #2.
    harness.check(list.getClass().getComponentType(), L1.class); // #3.

    list = ell.getListeners(L2.class);
    harness.check(list.length, 1); // #4.
    harness.check(list[0] == l2); // #5.
    harness.check(list.getClass().getComponentType(), L2.class); // #6.

    list = ell.getListeners(L3.class);
    harness.check(list.length, 1); // #7.
    harness.check(list[0] == l3); // #8.
    harness.check(list.getClass().getComponentType(), L3.class); // #9.

    list = ell.getListeners(EventListener.class);
    harness.check(list.length, 1); // #10.
    harness.check(list[0] == l1a); // #11.
    harness.check(list.getClass().getComponentType(),
                  EventListener.class); // #12.

    harness.check(ell.getListeners(L4.class).length, 0); // #13.

    // Check #14.
    caught = null;
    try
      {
        ell.getListeners(null);
      }
    catch (Exception ex)
      {
        caught = ex;
      }
    harness.check(caught instanceof NullPointerException);

  /* Doesn't compile with 1.5
    // Check #15.
    caught = null;
    try
      {
        ell.getListeners(String.class);
      }
    catch (Exception ex)
      {
        caught = ex;
      }
    harness.check(caught instanceof ClassCastException);
  */
  }
}
