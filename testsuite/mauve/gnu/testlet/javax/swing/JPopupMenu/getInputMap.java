/* getInputMap.java -- some checks for the getInputMap() methods inherited by
       the JPopupMenu class.
   Copyright (C) 2006 David Gilbert <david.gilbert@object-refinery.com>
This file is part of Mauve.

Mauve is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Mauve is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Mauve; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

*/

// Tags: JDK1.4

package gnu.testlet.javax.swing.JPopupMenu;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.InputMap;
import javax.swing.JComponent;
import javax.swing.JPopupMenu;

/**
 * Checks the input maps defined for a JPopupMenu - this is really testing
 * the setup performed by BasicPopupMenuUI.
 */
public class getInputMap implements Testlet
{
  public void test(TestHarness harness)
  {
    testMethod1(harness);
    testMethod2(harness);
  }
    
  public void testMethod1(TestHarness harness)
  {
    harness.checkPoint("()");
    JPopupMenu p = new JPopupMenu();
    InputMap m1 = p.getInputMap();
    InputMap m2 = p.getInputMap(JComponent.WHEN_FOCUSED);   
    harness.check(m1 == m2);
  }
    
  public void testMethod2(TestHarness harness)
  {
    harness.checkPoint("(int)");
    JPopupMenu p = new JPopupMenu();
    InputMap m1 = p.getInputMap(JComponent.WHEN_FOCUSED);
    harness.check(m1.keys(), null);
    InputMap m1p = m1.getParent();
    harness.check(m1p, null);
    InputMap m2 = p.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);   
    harness.check(m2.keys(), null);
    harness.check(m2.getParent(), null);
    InputMap m3 = p.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
    harness.check(m3.keys(), null);
    harness.check(m3.getParent(), null);
//    InputMap m3p = m3.getParent();
//    harness.check(m3p.get(KeyStroke.getKeyStroke("pressed ESCAPE")), "close");
//    for (int i = 0; i < m3p.keys().length; i++)
//    {
//      System.out.println(m3p.keys()[i] + ", " + m3p.get(m3p.keys()[i]));
//    }
  }
}
