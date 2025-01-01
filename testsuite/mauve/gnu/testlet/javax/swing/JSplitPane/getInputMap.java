/* getInputMap.java -- some checks for the getInputMap() method in the
       JSplitPane class.
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

// Tags: JDK1.3

package gnu.testlet.javax.swing.JSplitPane;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.InputMap;
import javax.swing.JComponent;
import javax.swing.JSplitPane;
import javax.swing.KeyStroke;

public class getInputMap implements Testlet
{
  public void test(TestHarness harness)
  {
    JSplitPane sp = new JSplitPane();
    InputMap m1 = sp.getInputMap(JComponent.WHEN_FOCUSED);
    harness.check(m1.keys(), null);
    harness.check(m1.getParent(), null);
    InputMap m2 = sp.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
    harness.check(m2.keys(), null);
    InputMap m2p = m2.getParent();
    harness.check(m2p.get(KeyStroke.getKeyStroke("shift ctrl pressed TAB")), "focusOutBackward");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed KP_RIGHT")), "positiveIncrement");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed DOWN")), "positiveIncrement");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed KP_LEFT")), "negativeIncrement");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed RIGHT")), "positiveIncrement");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed KP_DOWN")), "positiveIncrement");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed UP")), "negativeIncrement");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed KP_UP")), "negativeIncrement");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed F8")), "startResize");
    harness.check(m2p.get(KeyStroke.getKeyStroke("ctrl pressed TAB")), "focusOutForward");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed LEFT")), "negativeIncrement");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed HOME")), "selectMin");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed F6")), "toggleFocus");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed END")), "selectMax");
    InputMap m3 = sp.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
    harness.check(m3.keys(), null);
    harness.check(m3.getParent(), null);
  }
}
