/* getInputMap.java -- some checks for the getInputMap() methods inherited by
       the JScrollBar class.
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

package gnu.testlet.javax.swing.JScrollBar;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.InputMap;
import javax.swing.JComponent;
import javax.swing.JScrollBar;
import javax.swing.KeyStroke;

/**
 * Checks the input maps defined for a JScrollBar - this is really testing
 * the setup performed by BasicScrollBarUI.
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
    JScrollBar sb = new JScrollBar();
    InputMap m1 = sb.getInputMap();
    InputMap m2 = sb.getInputMap(JComponent.WHEN_FOCUSED);   
    harness.check(m1 == m2);
  }
    
  public void testMethod2(TestHarness harness)
  {
    harness.checkPoint("(int)");
    JScrollBar sb = new JScrollBar();
    InputMap m1 = sb.getInputMap(JComponent.WHEN_FOCUSED);
    harness.check(m1.keys(), null);
    InputMap m1p = m1.getParent();
    harness.check(m1p, null);
    InputMap m2 = sb.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);   
    harness.check(m2.keys(), null);
    InputMap m2p = m2.getParent();
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed PAGE_UP")), "negativeBlockIncrement");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed PAGE_DOWN")), "positiveBlockIncrement");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed END")), "maxScroll");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed HOME")), "minScroll");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed LEFT")), "negativeUnitIncrement");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed KP_UP")), "negativeUnitIncrement");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed KP_DOWN")), "positiveUnitIncrement");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed UP")), "negativeUnitIncrement");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed RIGHT")), "positiveUnitIncrement");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed KP_LEFT")), "negativeUnitIncrement");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed DOWN")), "positiveUnitIncrement");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed KP_RIGHT")), "positiveUnitIncrement");

    InputMap m3 = sb.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
    harness.check(m3.keys(), null);
    harness.check(m3.getParent(), null);
  }
}
