/* getInputMap.java -- some checks for the getInputMap() methods in the JList
       class.
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

package gnu.testlet.javax.swing.JList;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.InputMap;
import javax.swing.JComponent;
import javax.swing.JList;
import javax.swing.KeyStroke;

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
    JList list = new JList();
    InputMap m1 = list.getInputMap();
    InputMap m2 = list.getInputMap(JComponent.WHEN_FOCUSED);   
    harness.check(m1 == m2);
  }
  
  public void testMethod2(TestHarness harness)
  {
    harness.checkPoint("(int)");
    JList list = new JList();
    InputMap m1 = list.getInputMap(JComponent.WHEN_FOCUSED);
    harness.check(m1.keys(), null);
    InputMap m1p = m1.getParent();
    harness.check(m1p.get(KeyStroke.getKeyStroke("ctrl pressed DOWN")), 
            "selectNextRowChangeLead");
    harness.check(m1p.get(KeyStroke.getKeyStroke("shift pressed UP")), 
            "selectPreviousRowExtendSelection");
    harness.check(m1p.get(KeyStroke.getKeyStroke("ctrl pressed RIGHT")), 
            "selectNextColumnChangeLead");
    harness.check(m1p.get(KeyStroke.getKeyStroke("shift ctrl pressed LEFT")), 
            "selectPreviousColumnExtendSelection");
    harness.check(m1p.get(KeyStroke.getKeyStroke("shift pressed KP_UP")), 
            "selectPreviousRowExtendSelection");
    harness.check(m1p.get(KeyStroke.getKeyStroke("pressed DOWN")), 
            "selectNextRow");
    harness.check(m1p.get(KeyStroke.getKeyStroke("ctrl pressed UP")), 
            "selectPreviousRowChangeLead");
    harness.check(m1p.get(KeyStroke.getKeyStroke("ctrl pressed LEFT")), 
            "selectPreviousColumnChangeLead");
    harness.check(m1p.get(KeyStroke.getKeyStroke("pressed CUT")), 
            "cut");
    harness.check(m1p.get(KeyStroke.getKeyStroke("pressed END")), 
            "selectLastRow");
    harness.check(m1p.get(KeyStroke.getKeyStroke("shift pressed PAGE_UP")), 
            "scrollUpExtendSelection");
    harness.check(m1p.get(KeyStroke.getKeyStroke("pressed KP_UP")), 
            "selectPreviousRow");
    harness.check(m1p.get(KeyStroke.getKeyStroke("shift ctrl pressed UP")), 
            "selectPreviousRowExtendSelection");
    harness.check(m1p.get(KeyStroke.getKeyStroke("ctrl pressed HOME")),
            "selectFirstRowChangeLead");
    harness.check(m1p.get(KeyStroke.getKeyStroke("shift pressed LEFT")), 
            "selectPreviousColumnExtendSelection");
    harness.check(m1p.get(KeyStroke.getKeyStroke("ctrl pressed END")), 
            "selectLastRowChangeLead");
    harness.check(m1p.get(KeyStroke.getKeyStroke("ctrl pressed PAGE_DOWN")), 
            "scrollDownChangeLead");
    harness.check(m1p.get(KeyStroke.getKeyStroke("shift ctrl pressed RIGHT")), 
            "selectNextColumnExtendSelection");
    harness.check(m1p.get(KeyStroke.getKeyStroke("pressed LEFT")), 
            "selectPreviousColumn");
    harness.check(m1p.get(KeyStroke.getKeyStroke("ctrl pressed PAGE_UP")), 
            "scrollUpChangeLead");
    harness.check(m1p.get(KeyStroke.getKeyStroke("pressed KP_LEFT")), 
            "selectPreviousColumn");
    harness.check(m1p.get(KeyStroke.getKeyStroke("shift pressed KP_RIGHT")), 
            "selectNextColumnExtendSelection");
    harness.check(m1p.get(KeyStroke.getKeyStroke("pressed SPACE")), 
            "addToSelection");
    harness.check(m1p.get(KeyStroke.getKeyStroke("ctrl pressed SPACE")), 
            "toggleAndAnchor");
    harness.check(m1p.get(KeyStroke.getKeyStroke("shift pressed SPACE")), 
            "extendTo");
    harness.check(m1p.get(KeyStroke.getKeyStroke("shift ctrl pressed SPACE")), 
            "moveSelectionTo");
    harness.check(m1p.get(KeyStroke.getKeyStroke("shift ctrl pressed DOWN")), 
            "selectNextRowExtendSelection");
    harness.check(m1p.get(KeyStroke.getKeyStroke("ctrl pressed BACK_SLASH")), 
            "clearSelection");
    harness.check(m1p.get(KeyStroke.getKeyStroke("shift pressed HOME")), 
            "selectFirstRowExtendSelection");
    harness.check(m1p.get(KeyStroke.getKeyStroke("pressed RIGHT")), 
            "selectNextColumn");
    harness.check(m1p.get(KeyStroke.getKeyStroke("shift ctrl pressed PAGE_UP")),
            "scrollUpExtendSelection");
    harness.check(m1p.get(KeyStroke.getKeyStroke("shift pressed DOWN")), 
            "selectNextRowExtendSelection");
    harness.check(m1p.get(KeyStroke.getKeyStroke("pressed PAGE_DOWN")), 
            "scrollDown");
    harness.check(m1p.get(KeyStroke.getKeyStroke("shift ctrl pressed KP_UP")), 
            "selectPreviousRowExtendSelection");
    harness.check(m1p.get(KeyStroke.getKeyStroke("shift pressed KP_LEFT")), 
            "selectPreviousColumnExtendSelection");
    harness.check(m1p.get(KeyStroke.getKeyStroke("ctrl pressed X")), 
            "cut");
    harness.check(m1p.get(KeyStroke.getKeyStroke(
            "shift ctrl pressed PAGE_DOWN")), "scrollDownExtendSelection");
    harness.check(m1p.get(KeyStroke.getKeyStroke("ctrl pressed SLASH")), 
            "selectAll");
    harness.check(m1p.get(KeyStroke.getKeyStroke("ctrl pressed C")), "copy");
    harness.check(m1p.get(KeyStroke.getKeyStroke("ctrl pressed KP_RIGHT")), 
            "selectNextColumnChangeLead");
    harness.check(m1p.get(KeyStroke.getKeyStroke("shift pressed END")), 
            "selectLastRowExtendSelection");
    harness.check(m1p.get(KeyStroke.getKeyStroke("shift ctrl pressed KP_DOWN")),
            "selectNextRowExtendSelection");
    harness.check(m1p.get(KeyStroke.getKeyStroke("ctrl pressed KP_LEFT")), 
            "selectPreviousColumnChangeLead");
    harness.check(m1p.get(KeyStroke.getKeyStroke("pressed HOME")), 
            "selectFirstRow");
    harness.check(m1p.get(KeyStroke.getKeyStroke("ctrl pressed V")), "paste");
    harness.check(m1p.get(KeyStroke.getKeyStroke("pressed KP_DOWN")), 
            "selectNextRow");
    harness.check(m1p.get(KeyStroke.getKeyStroke("ctrl pressed KP_DOWN")), 
            "selectNextRowChangeLead");
    harness.check(m1p.get(KeyStroke.getKeyStroke("shift pressed RIGHT")), 
            "selectNextColumnExtendSelection");
    harness.check(m1p.get(KeyStroke.getKeyStroke("ctrl pressed A")), 
            "selectAll");
    harness.check(m1p.get(KeyStroke.getKeyStroke("shift ctrl pressed END")), 
            "selectLastRowExtendSelection");
    harness.check(m1p.get(KeyStroke.getKeyStroke("pressed COPY")), "copy");
    harness.check(m1p.get(KeyStroke.getKeyStroke("ctrl pressed KP_UP")), 
            "selectPreviousRowChangeLead");
    harness.check(m1p.get(KeyStroke.getKeyStroke("shift ctrl pressed KP_LEFT")), 
            "selectPreviousColumnExtendSelection");
    harness.check(m1p.get(KeyStroke.getKeyStroke("shift pressed KP_DOWN")),
            "selectNextRowExtendSelection");
    harness.check(m1p.get(KeyStroke.getKeyStroke("pressed UP")), 
            "selectPreviousRow");
    harness.check(m1p.get(KeyStroke.getKeyStroke("shift ctrl pressed HOME")), 
            "selectFirstRowExtendSelection");
    harness.check(m1p.get(KeyStroke.getKeyStroke("shift pressed PAGE_DOWN")), 
            "scrollDownExtendSelection");
    harness.check(m1p.get(KeyStroke.getKeyStroke("pressed KP_RIGHT")), 
            "selectNextColumn");
    harness.check(m1p.get(KeyStroke.getKeyStroke(
            "shift ctrl pressed KP_RIGHT")),
            "selectNextColumnExtendSelection");
    harness.check(m1p.get(KeyStroke.getKeyStroke("pressed PAGE_UP")), 
            "scrollUp");
    harness.check(m1p.get(KeyStroke.getKeyStroke("pressed PASTE")), "paste");
    InputMap m2 = list.getInputMap(
            JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);   
    harness.check(m2.keys(), null);
    harness.check(m2.getParent(), null);
    InputMap m3 = list.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
    harness.check(m3.keys(), null);
    harness.check(m3.getParent(), null);
  }
}
