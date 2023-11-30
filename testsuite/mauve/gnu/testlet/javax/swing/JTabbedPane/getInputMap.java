/* getInputMap.java -- some checks for the getInputMap() methods inherited by
       the JTabbedPane class.
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

package gnu.testlet.javax.swing.JTabbedPane;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.InputMap;
import javax.swing.JComponent;
import javax.swing.JTabbedPane;
import javax.swing.KeyStroke;

/**
 * Checks the input maps defined for a JTabbedPane - this is really testing
 * the setup performed by BasicTabbedPaneUI.
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
    JTabbedPane tp = new JTabbedPane();
    InputMap m1 = tp.getInputMap();
    InputMap m2 = tp.getInputMap(JComponent.WHEN_FOCUSED);   
    harness.check(m1 == m2);
  }
    
  public void testMethod2(TestHarness harness)
  {
    harness.checkPoint("(int)");
    JTabbedPane tp = new JTabbedPane();
    InputMap m1 = tp.getInputMap(JComponent.WHEN_FOCUSED);
    harness.check(m1.keys(), null);
    InputMap m1p = m1.getParent();
    harness.check(m1p.get(KeyStroke.getKeyStroke("ctrl pressed DOWN")), 
            "requestFocusForVisibleComponent");
    harness.check(m1p.get(KeyStroke.getKeyStroke("pressed KP_UP")), 
            "navigateUp");
    harness.check(m1p.get(KeyStroke.getKeyStroke("pressed LEFT")), 
            "navigateLeft");
    harness.check(m1p.get(KeyStroke.getKeyStroke("ctrl pressed KP_DOWN")), 
            "requestFocusForVisibleComponent");
    harness.check(m1p.get(KeyStroke.getKeyStroke("pressed UP")), "navigateUp");
    harness.check(m1p.get(KeyStroke.getKeyStroke("pressed KP_DOWN")), 
            "navigateDown");
    harness.check(m1p.get(KeyStroke.getKeyStroke("pressed KP_LEFT")), 
            "navigateLeft");
    harness.check(m1p.get(KeyStroke.getKeyStroke("pressed RIGHT")), 
            "navigateRight");
    harness.check(m1p.get(KeyStroke.getKeyStroke("pressed KP_RIGHT")), 
            "navigateRight");
    harness.check(m1p.get(KeyStroke.getKeyStroke("pressed DOWN")), 
            "navigateDown");
    InputMap m2 = tp.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);   
    harness.check(m2.keys(), null);
    InputMap m2p = m2.getParent();
    harness.check(m2p.get(KeyStroke.getKeyStroke("ctrl pressed PAGE_DOWN")), 
            "navigatePageDown");
    harness.check(m2p.get(KeyStroke.getKeyStroke("ctrl pressed PAGE_UP")), 
            "navigatePageUp");
    harness.check(m2p.get(KeyStroke.getKeyStroke("ctrl pressed UP")), 
            "requestFocus");
    harness.check(m2p.get(KeyStroke.getKeyStroke("ctrl pressed KP_UP")), 
            "requestFocus");
    InputMap m3 = tp.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
    harness.check(m3.keys(), null);
    harness.check(m3.getParent(), null);
  }
}
