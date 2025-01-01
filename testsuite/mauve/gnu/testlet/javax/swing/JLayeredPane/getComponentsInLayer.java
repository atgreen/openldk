//Tags: JDK1.2

//Copyright (C) 2005 Roman Kennke <kennke@aicas.com>

//This file is part of Mauve.

//Mauve is free software; you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation; either version 2, or (at your option)
//any later version.

//Mauve is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.

//You should have received a copy of the GNU General Public License
//along with Mauve; see the file COPYING.  If not, write to
//the Free Software Foundation, 59 Temple Place - Suite 330,
//Boston, MA 02111-1307, USA.

package gnu.testlet.javax.swing.JLayeredPane;

import javax.swing.JLabel;
import javax.swing.JLayeredPane;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Checks if the method getComponentsInLayer works as expected.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class getComponentsInLayer implements Testlet
{

  /**
   * Starts the test run.
   *
   * @param harness the test harness to use
   */
  public void test(TestHarness harness)
  {
    testUnknownLayer(harness);
  }

  /**
   * Checks what this method returns for a layer that is not in the
   * JLayeredPane. The method should always return an empty Component[] for
   * all unknown layers (even with negative layer numbers).
   *
   * @param harness the test harness to use
   */
  private void testUnknownLayer(TestHarness harness)
  {
    harness.checkPoint("unknownLayer");
    JLayeredPane l = new JLayeredPane();
    // We add one component to layer 1 and one to layer 3 and check what
    // happens when we request the components in layer -1 (negative), 0
    // (positive and less then the minimum layer), 2 (between the layers),
    // 4 (greater than the maximum layer).
    JLabel l1 = new JLabel("Hello");
    l.setLayer(l1, 1);
    l.add(l1);
    JLabel l2 = new JLabel("World");
    l.setLayer(l2, 3);
    l.add(l2);
    harness.check(l.getComponentsInLayer(-1).length, 0);
    harness.check(l.getComponentsInLayer(-0).length, 0);
    harness.check(l.getComponentsInLayer(-2).length, 0);
    harness.check(l.getComponentsInLayer(-4).length, 0);
  }
}
