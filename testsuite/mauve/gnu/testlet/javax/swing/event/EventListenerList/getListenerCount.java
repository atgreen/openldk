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
public class getListenerCount
  implements Testlet
{
  private static class L1 implements EventListener {};
  private static class L2 extends L1 {};
  private static class L3 extends L1 {};

  public void test(TestHarness harness)
  {
    EventListenerList ell = new EventListenerList();
    L1 l1 = new L1();
    L2 l2 = new L2();
    L3 l3_1 = new L3();
    L3 l3_2 = new L3();

    // Check #1.
    harness.check(ell.getListenerCount(), 0);

    // Check #2.
    harness.check(ell.getListenerCount(L1.class), 0);

    // Check #3: null argument (Classpath bug #7099).
    harness.check(ell.getListenerCount(null), 0);

    // Check #4: Class that does not implement EventListener.
    harness.check(ell.getListenerCount(String.class), 0);

    // Check #5.
    ell.add(L1.class, l1);
    ell.add(L2.class, l2);
    ell.add(L1.class, l3_1);
    ell.add(L3.class, l3_2);
    harness.check(ell.getListenerCount(), 4);

    // Check #6.
    harness.check(ell.getListenerCount(L1.class), 2);

    // Check #7.
    harness.check(ell.getListenerCount(L2.class), 1);

    // Check #8.
    harness.check(ell.getListenerCount(L3.class), 1);
  }
}
