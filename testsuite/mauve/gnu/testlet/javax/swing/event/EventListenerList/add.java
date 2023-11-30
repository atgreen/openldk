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
 * These tests pass with the Sun JDK 1.4.1_01 on GNU/Linux IA-32.
 *
 * @see Classpath bug #7104
 *
 * @author Sascha Brawer (brawer@dandelis.ch)
 */
public class add
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
    L l1 = new L();
    L l2 = new L();
    Object[] list;

    // Check #1.
    ell.add(EventListener.class, l1);
    list = ell.getListenerList();
    harness.check(list.length, 2);
    
    // Check #2.
    harness.check(list[0], EventListener.class);

    // Check #3.
    harness.check(list[1] == l1);

    // Check #4.
    ell.add(L.class, l2);
    list = ell.getListenerList();
    harness.check(list.length, 4);

    // Check #5.
    // Classpath bug #7104.
    harness.check(list[0], EventListener.class);

    // Check #6.
    // Classpath bug #7104.
    harness.check(list[1] == l1);

    // Check #7.
    harness.check(list[2], L.class);

    // Check #8.
    harness.check(list[3] == l2);

    // Check #9: Adding null listener.
    ell = new EventListenerList();    
    Throwable caught = null;
    try
      {
        ell.add(L.class, null);
      }
    catch (Exception ex)
      {
        caught = ex;
      }
    harness.check(caught, null);

    // Check #10: Adding null listener does not change anything.
    // Classpath bug #7104.
    harness.check(ell.getListenerCount(), 0);

    // Check #11: Registering as null class.
    caught = null;
    try
      {
        ell.add(null, new L());
      }
    catch (Exception ex)
      {
        caught = ex;
      }
    harness.check(caught != null);

    // Check #12: Registering a null class does not actually
    // register it.
    // Classpath bug #7104.
    harness.check(ell.getListenerCount(), 0);
    
    // Check #13: Registering a non-instance.
    // Classpath bug #7104.
    caught = null;
    try
      {
        ell.add( (Class) L.class, new L2());
      }
    catch (Exception ex)
      {
        caught = ex;
      }
    harness.check(caught instanceof IllegalArgumentException);

    // Check #14: Registering the same listener twice.
    // Classpath bug #7104.
    ell.add(L.class, l1);
    ell.add(L.class, l1);
    harness.check(ell.getListenerCount(), 2);
  }
}
