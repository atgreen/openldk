/* getInputMap.java -- some checks for the getInputMap() methods inherited by
       the JTable class.
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

package gnu.testlet.javax.swing.JTable;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.InputMap;
import javax.swing.JComponent;
import javax.swing.JTable;
import javax.swing.KeyStroke;

/**
 * Checks the input maps defined for a JTable - this is really testing
 * the setup performed by BasicTableUI.
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
    JTable t = new JTable();
    InputMap m1 = t.getInputMap();
    InputMap m2 = t.getInputMap(JComponent.WHEN_FOCUSED);   
    harness.check(m1 == m2);
  }
    
  public void testMethod2(TestHarness harness)
  {
    harness.checkPoint("(int)");
    JTable t = new JTable();
    InputMap m1 = t.getInputMap(JComponent.WHEN_FOCUSED);
    harness.check(m1.keys(), null);
    harness.check(m1.getParent(), null);
    InputMap m2 = t.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);   
    harness.check(m2.keys(), null);
    InputMap m2p = m2.getParent();
    harness.check(m2p.get(KeyStroke.getKeyStroke("ctrl pressed DOWN")), 
            "selectNextRowChangeLead");
    harness.check(m2p.get(KeyStroke.getKeyStroke("shift pressed UP")), 
            "selectPreviousRowExtendSelection");
    harness.check(m2p.get(KeyStroke.getKeyStroke("ctrl pressed RIGHT")), 
            "selectNextColumnChangeLead");
    harness.check(m2p.get(KeyStroke.getKeyStroke("shift ctrl pressed LEFT")), 
            "selectPreviousColumnExtendSelection");
    harness.check(m2p.get(KeyStroke.getKeyStroke("shift pressed KP_UP")), 
            "selectPreviousRowExtendSelection");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed DOWN")), 
            "selectNextRow");
    harness.check(m2p.get(KeyStroke.getKeyStroke("ctrl pressed UP")), 
            "selectPreviousRowChangeLead");
    harness.check(m2p.get(KeyStroke.getKeyStroke("ctrl pressed LEFT")), 
            "selectPreviousColumnChangeLead");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed CUT")), "cut");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed END")), 
            "selectLastColumn");
    harness.check(m2p.get(KeyStroke.getKeyStroke("shift pressed PAGE_UP")), 
            "scrollUpExtendSelection");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed KP_UP")), 
            "selectPreviousRow");
    harness.check(m2p.get(KeyStroke.getKeyStroke("shift ctrl pressed UP")), 
            "selectPreviousRowExtendSelection");
    harness.check(m2p.get(KeyStroke.getKeyStroke("ctrl pressed HOME")), 
            "selectFirstRow");
    harness.check(m2p.get(KeyStroke.getKeyStroke("shift pressed LEFT")), 
            "selectPreviousColumnExtendSelection");
    harness.check(m2p.get(KeyStroke.getKeyStroke("ctrl pressed END")), 
            "selectLastRow");
    harness.check(m2p.get(KeyStroke.getKeyStroke("ctrl pressed PAGE_DOWN")), 
            "scrollRightChangeSelection");
    harness.check(m2p.get(KeyStroke.getKeyStroke("shift ctrl pressed RIGHT")), 
            "selectNextColumnExtendSelection");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed LEFT")), 
            "selectPreviousColumn");
    harness.check(m2p.get(KeyStroke.getKeyStroke("ctrl pressed PAGE_UP")), 
            "scrollLeftChangeSelection");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed KP_LEFT")), 
            "selectPreviousColumn");
    harness.check(m2p.get(KeyStroke.getKeyStroke("shift pressed KP_RIGHT")), 
            "selectNextColumnExtendSelection");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed SPACE")), 
            "addToSelection");
    harness.check(m2p.get(KeyStroke.getKeyStroke("ctrl pressed SPACE")), 
            "toggleAndAnchor");
    harness.check(m2p.get(KeyStroke.getKeyStroke("shift pressed SPACE")), 
            "extendTo");
    harness.check(m2p.get(KeyStroke.getKeyStroke("shift ctrl pressed SPACE")), 
            "moveSelectionTo");
    harness.check(m2p.get(KeyStroke.getKeyStroke("shift ctrl pressed DOWN")), 
            "selectNextRowExtendSelection");
    harness.check(m2p.get(KeyStroke.getKeyStroke("ctrl pressed BACK_SLASH")), 
            "clearSelection");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed ESCAPE")), 
            "cancel");
    harness.check(m2p.get(KeyStroke.getKeyStroke("shift pressed HOME")), 
            "selectFirstColumnExtendSelection");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed ENTER")), 
            "selectNextRowCell");
    harness.check(m2p.get(KeyStroke.getKeyStroke("shift pressed ENTER")), 
            "selectPreviousRowCell");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed RIGHT")), 
            "selectNextColumn");
    harness.check(m2p.get(KeyStroke.getKeyStroke("shift ctrl pressed PAGE_UP")),
            "scrollLeftExtendSelection");
    harness.check(m2p.get(KeyStroke.getKeyStroke("shift pressed DOWN")), 
            "selectNextRowExtendSelection");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed PAGE_DOWN")), 
            "scrollDownChangeSelection");
    harness.check(m2p.get(KeyStroke.getKeyStroke("shift ctrl pressed KP_UP")), 
            "selectPreviousRowExtendSelection");
    harness.check(m2p.get(KeyStroke.getKeyStroke("shift pressed KP_LEFT")), 
            "selectPreviousColumnExtendSelection");
    harness.check(m2p.get(KeyStroke.getKeyStroke("ctrl pressed X")), "cut");
    harness.check(m2p.get(KeyStroke.getKeyStroke(
            "shift ctrl pressed PAGE_DOWN")), "scrollRightExtendSelection");
    harness.check(m2p.get(KeyStroke.getKeyStroke("ctrl pressed SLASH")),
            "selectAll");
    harness.check(m2p.get(KeyStroke.getKeyStroke("ctrl pressed C")), "copy");
    harness.check(m2p.get(KeyStroke.getKeyStroke("ctrl pressed KP_RIGHT")), 
            "selectNextColumnChangeLead");
    harness.check(m2p.get(KeyStroke.getKeyStroke("shift pressed END")), 
            "selectLastColumnExtendSelection");
    harness.check(m2p.get(KeyStroke.getKeyStroke(
            "shift ctrl pressed KP_DOWN")), "selectNextRowExtendSelection");
    harness.check(m2p.get(KeyStroke.getKeyStroke("shift pressed TAB")), 
            "selectPreviousColumnCell");
    harness.check(m2p.get(KeyStroke.getKeyStroke("ctrl pressed KP_LEFT")), 
            "selectPreviousColumnChangeLead");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed HOME")), 
            "selectFirstColumn");
    harness.check(m2p.get(KeyStroke.getKeyStroke("ctrl pressed V")), 
            "paste");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed KP_DOWN")), 
            "selectNextRow");
    harness.check(m2p.get(KeyStroke.getKeyStroke("ctrl pressed KP_DOWN")), 
            "selectNextRowChangeLead");
    harness.check(m2p.get(KeyStroke.getKeyStroke("shift pressed RIGHT")), 
            "selectNextColumnExtendSelection");
    harness.check(m2p.get(KeyStroke.getKeyStroke("ctrl pressed A")), 
            "selectAll");
    harness.check(m2p.get(KeyStroke.getKeyStroke("shift ctrl pressed END")), 
            "selectLastRowExtendSelection");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed COPY")), "copy");
    harness.check(m2p.get(KeyStroke.getKeyStroke("ctrl pressed KP_UP")), 
            "selectPreviousRowChangeLead");
    harness.check(m2p.get(KeyStroke.getKeyStroke(
            "shift ctrl pressed KP_LEFT")), 
            "selectPreviousColumnExtendSelection");
    harness.check(m2p.get(KeyStroke.getKeyStroke("shift pressed KP_DOWN")), 
            "selectNextRowExtendSelection");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed TAB")), 
            "selectNextColumnCell");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed UP")), 
            "selectPreviousRow");
    harness.check(m2p.get(KeyStroke.getKeyStroke("shift ctrl pressed HOME")), 
            "selectFirstRowExtendSelection");
    harness.check(m2p.get(KeyStroke.getKeyStroke("shift pressed PAGE_DOWN")), 
            "scrollDownExtendSelection");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed KP_RIGHT")), 
            "selectNextColumn");
    harness.check(m2p.get(KeyStroke.getKeyStroke(
            "shift ctrl pressed KP_RIGHT")), "selectNextColumnExtendSelection");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed F2")), 
            "startEditing");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed PAGE_UP")), 
            "scrollUpChangeSelection");
    harness.check(m2p.get(KeyStroke.getKeyStroke("pressed PASTE")), "paste");
    InputMap m3 = t.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
    harness.check(m3.keys(), null);
    harness.check(m3.getParent(), null);
  }
}
