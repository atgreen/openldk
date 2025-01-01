// Tags: JDK1.2

// Copyright (C) 2005 Red Hat

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
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.javax.swing.SwingUtilities;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.ActionMap;
import javax.swing.JComponent;
import javax.swing.JTable;
import javax.swing.SwingUtilities;
import javax.swing.plaf.ActionMapUIResource;


/**
 * Some checks for the layoutCompoundLabel() method in the SwingUtilities
 * class.
 */
public class replaceUIActionMap implements Testlet
{

  static int howManyMaps (JComponent t)
  {
    ActionMap curr = t.getActionMap();
    int num = 0;
    while (curr != null)
      {
        num ++;
        curr = curr.getParent();
      }
    return num;
  }

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness) 
  {
    JTable t = new JTable();
    harness.check(howManyMaps(t) == 2);
    
    ActionMap map1 = SwingUtilities.getUIActionMap(t);
    map1.setParent(new ActionMap());

    harness.check(howManyMaps(t) == 3);

    ActionMapUIResource map2 = new ActionMapUIResource();
    SwingUtilities.replaceUIActionMap(t, map2);

    harness.check(howManyMaps(t) == 2);
  }
}
