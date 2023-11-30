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
public class remove
  implements Testlet
{
  private static class L
    implements EventListener
  {
  };

  private static class L2
    implements EventListener
  {
  };

  public void test(TestHarness harness)
  {
    EventListenerList ell = new EventListenerList();
    EventListener l1 = new L();
    L l2 = new L();
    Object[] list;
    Throwable caught;

    // Check #1.
    ell.add(EventListener.class, l1);
    ell.add(L.class, l2);
    ell.remove(EventListener.class, l1);
    list = ell.getListenerList();
    harness.check(list.length, 2);

    // Check #2.
    // Classpath bug #7105.
    harness.check(list[0] == L.class);

    // Check #3.
    // Classpath bug #7105.
    harness.check(list[1] == l2);

    // Check #4: Remove something not in the list.
    ell.remove(EventListener.class, l2);
    list = ell.getListenerList();
    harness.check(list.length, 2);

    // Check #5: remove(null, foo)
    // Classpath bug #7105.
    caught = null;
    try
      {
        ell.remove(null, l2);
      }
    catch (Exception ex)
      {
        caught = ex;
      }
    harness.check(caught instanceof NullPointerException);

    // Check #6: remove(foo, null)
    caught = null;
    try
      {
        ell.remove(EventListener.class, null);
      }
    catch (Exception ex)
      {
        caught = ex;
      }
    harness.check(caught, null);

    // Check #7: remove(null, null)
    caught = null;
    try
      {
        ell.remove(null, null);
      }
    catch (Exception ex)
      {
        caught = ex;
      }
    harness.check(caught, null);

  /* Doesn't compile with 1.5
    // Check #8: Removing non-instance.
    // Classpath bug #7105.
    caught = null;
    try
      {
        ell.remove(L2.class, l2);
      }
    catch (Exception ex)
      {
        caught = ex;
      }
    harness.check(caught instanceof IllegalArgumentException);
  */

    // Unsuccessful attempts should not change the list.
    list = ell.getListenerList();    
    harness.check(list.length, 2);   // Check #9.
    harness.check(list[0], L.class); // Check #10. Classpath bug #7105.
    harness.check(list[1] == l2);    // Check #11. Classpath bug #7105.

    // Check #12: Removal of doubly registered listener.
    ell = new EventListenerList();
    ell.add(L.class, l2);
    ell.add(L.class, l2);
    ell.remove(L.class, l2);
    harness.check(ell.getListenerList().length, 2);
  }
}
