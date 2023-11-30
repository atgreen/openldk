/* getInputMap.java -- some checks for the getInputMap() methods inherited by
       the JScrollPane class.
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

package gnu.testlet.javax.swing.JScrollPane;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.InputMap;
import javax.swing.JComponent;
import javax.swing.JScrollPane;
import javax.swing.KeyStroke;

/**
 * Checks the input maps defined for a JScrollPane - this is really testing
 * the setup performed by BasicScrollPaneUI.
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
    JScrollPane sp = new JScrollPane();
    InputMap m1 = sp.getInputMap();
    InputMap m2 = sp.getInputMap(JComponent.WHEN_FOCUSED);   
    harness.check(m1 == m2);
  }
    
  public void testMethod2(TestHarness harness)
  {
    harness.checkPoint("(int)");
    JScrollPane sp = new JScrollPane();
    InputMap m1 = sp.getInputMap(JComponent.WHEN_FOCUSED);
    harness.check(m1.keys(), null);
    InputMap m1p = m1.getParent();
    harness.check(m1p, null);
    InputMap m2 = sp.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);   
    harness.check(m2.keys(), null);
    InputMap m2p = m2.getParent();
    harness.check(m2p.get(KeyStroke.getKeyStroke("ctrl pressed PAGE_DOWN")), "scrollRight");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed PAGE_DOWN")), "scrollDown");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed PAGE_UP")), "scrollUp");
    harness.check(m2p.get(KeyStroke.getKeyStroke("ctrl pressed PAGE_UP")), "scrollLeft");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed KP_RIGHT")), "unitScrollRight");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed DOWN")), "unitScrollDown");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed KP_LEFT")), "unitScrollLeft");
    harness.check(m2p.get(KeyStroke.getKeyStroke("ctrl pressed HOME")), "scrollHome");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed RIGHT")), "unitScrollRight");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed KP_DOWN")), "unitScrollDown");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed UP")), "unitScrollUp");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed KP_UP")), "unitScrollUp");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed LEFT")), "unitScrollLeft");
    harness.check(m2p.get(KeyStroke.getKeyStroke("ctrl pressed END")), "scrollEnd");
    InputMap m3 = sp.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
    harness.check(m3.keys(), null);
    harness.check(m3.getParent(), null);
  }
}
