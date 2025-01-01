/* clearSelection.java -- some checks for the clearSelection()
       method in the DefaultListSelectionModel class.
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

// Tags: JDK1.5

package gnu.testlet.javax.swing.DefaultListSelectionModel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.DefaultListSelectionModel;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

public class clearSelection implements Testlet, ListSelectionListener
{
  ListSelectionEvent lastEvent;
  
  public void valueChanged(ListSelectionEvent e) 
  {
    lastEvent = e;  
  }

  public void test(TestHarness harness)
  {
    testSingleSelection(harness);
    testSingleIntervalSelection(harness);
    testMultipleIntervalSelection(harness);
  }
  
  private void testSingleSelection(TestHarness harness)
  {
    // clearing an empty selection generates no event
    harness.checkPoint("SINGLE_SELECTION (1)");
    DefaultListSelectionModel m = new DefaultListSelectionModel();
    m.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    m.addListSelectionListener(this);
    m.clearSelection();
    harness.check(lastEvent, null);
    
    harness.checkPoint("SINGLE_SELECTION (2)");
    m.setSelectionInterval(3, 3);
    lastEvent = null;
    m.clearSelection();
    harness.check(m.isSelectionEmpty());
    harness.check(m.getAnchorSelectionIndex(), 3);
    harness.check(m.getLeadSelectionIndex(), 3);
    harness.check(lastEvent.getSource(), m);
    harness.check(lastEvent.getFirstIndex(), 3);
    harness.check(lastEvent.getLastIndex(), 3);
  }
  
  private void testSingleIntervalSelection(TestHarness harness)
  {
    // check 1 : clearing an empty selection generates no event
    harness.checkPoint("SINGLE_INTERVAL_SELECTION (1)");
    DefaultListSelectionModel m = new DefaultListSelectionModel();
    m.setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
    m.addListSelectionListener(this);
    harness.check(m.isSelectionEmpty(), true);
    lastEvent = null;
    m.clearSelection();
    harness.check(m.isSelectionEmpty(), true);
    harness.check(lastEvent, null);
    
    // check 2 : clearing a selection generates an event with first and last
    // indices covering the former selection
    harness.checkPoint("SINGLE_INTERVAL_SELECTION (2)");
    m.addSelectionInterval(10, 20);
    lastEvent = null;
    m.clearSelection();
    harness.check(m.isSelectionEmpty(), true);
    harness.check(m.isSelectedIndex(10), false);
    harness.check(m.getAnchorSelectionIndex(), 10);
    harness.check(m.getLeadSelectionIndex(), 20);
    harness.check(m.getMinSelectionIndex(), -1);
    harness.check(m.getMaxSelectionIndex(), -1);
    harness.check(lastEvent.getSource(), m);
    harness.check(lastEvent.getFirstIndex(), 10);
    harness.check(lastEvent.getLastIndex(), 20); 
  }

  private void testMultipleIntervalSelection(TestHarness harness)
  {
    // check 1 : clearing an empty selection generates no event
    harness.checkPoint("MULTIPLE_INTERVAL_SELECTION (1)");
    DefaultListSelectionModel m = new DefaultListSelectionModel();
    m.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
    m.addListSelectionListener(this);
    harness.check(m.isSelectionEmpty(), true);
    lastEvent = null;
    m.clearSelection();
    harness.check(m.isSelectionEmpty(), true);
    harness.check(lastEvent, null);
    
    // check 2 : clearing a selection generates an event with first and last
    // indices covering the former selection
    harness.checkPoint("MULTIPLE_INTERVAL_SELECTION (2)");
    m.addSelectionInterval(2, 3);
    m.addSelectionInterval(12, 13);
    lastEvent = null;
    m.clearSelection();
    harness.check(m.isSelectionEmpty(), true);
    harness.check(m.getAnchorSelectionIndex(), 12);
    harness.check(m.getLeadSelectionIndex(), 13);
    harness.check(m.getMinSelectionIndex(), -1);
    harness.check(m.getMaxSelectionIndex(), -1);
    harness.check(lastEvent.getSource(), m);
    harness.check(lastEvent.getFirstIndex(), 2);
    harness.check(lastEvent.getLastIndex(), 13); 
  }

}
