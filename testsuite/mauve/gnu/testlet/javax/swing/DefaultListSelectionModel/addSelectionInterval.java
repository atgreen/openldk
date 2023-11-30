/* addSelectionInterval.java -- some checks for the addSelectionInterval()
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

public class addSelectionInterval implements Testlet, ListSelectionListener
{
 
  ListSelectionEvent lastEvent;
    
  public void valueChanged(ListSelectionEvent event) 
  {
    lastEvent = event;
  }

  public void test(TestHarness harness)
  {
    testSingleSelection(harness);
    testSingleIntervalSelection(harness);
    testMultipleIntervalSelection(harness);
  }
  
  private void testSingleSelection(TestHarness harness)
  {
    harness.checkPoint("SINGLE_SELECTION");
    DefaultListSelectionModel m = new DefaultListSelectionModel();
    m.addListSelectionListener(this);
    m.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    m.addSelectionInterval(1, 3);
    harness.check(m.isSelectedIndex(0), false);
    harness.check(m.isSelectedIndex(1), false);
    harness.check(m.isSelectedIndex(2), false);
    harness.check(m.isSelectedIndex(3), true);
    harness.check(m.isSelectedIndex(4), false);
    harness.check(m.getAnchorSelectionIndex(), 3);
    harness.check(m.getLeadSelectionIndex(), 3);
    harness.check(m.getMaxSelectionIndex(), 3);
    harness.check(m.getMinSelectionIndex(), 3);
    harness.check(lastEvent.getSource(), m);
    harness.check(lastEvent.getFirstIndex(), 3);
    harness.check(lastEvent.getLastIndex(), 3);
    
    m.clearSelection();
    m.addSelectionInterval(3, 1);
    harness.check(m.isSelectedIndex(0), false);
    harness.check(m.isSelectedIndex(1), true);
    harness.check(m.isSelectedIndex(2), false);
    harness.check(m.isSelectedIndex(3), false);
    harness.check(m.isSelectedIndex(4), false);
    harness.check(m.getAnchorSelectionIndex(), 1);
    harness.check(m.getLeadSelectionIndex(), 1);
    harness.check(m.getMaxSelectionIndex(), 1);
    harness.check(m.getMinSelectionIndex(), 1);
    harness.check(lastEvent.getSource(), m);
    harness.check(lastEvent.getFirstIndex(), 1);
    harness.check(lastEvent.getLastIndex(), 3);
  }

  private void testSingleIntervalSelection(TestHarness harness)
  {
    harness.checkPoint("SINGLE_INTERVAL_SELECTION");
    DefaultListSelectionModel m = new DefaultListSelectionModel();
    m.addListSelectionListener(this);
    m.setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
    m.addSelectionInterval(1, 3);
    harness.check(m.isSelectedIndex(0), false);
    harness.check(m.isSelectedIndex(1), true);
    harness.check(m.isSelectedIndex(2), true);
    harness.check(m.isSelectedIndex(3), true);
    harness.check(m.isSelectedIndex(4), false);
    harness.check(m.getAnchorSelectionIndex(), 1);
    harness.check(m.getLeadSelectionIndex(), 3);
    harness.check(m.getMaxSelectionIndex(), 3);
    harness.check(m.getMinSelectionIndex(), 1);
    harness.check(lastEvent.getSource(), m);
    harness.check(lastEvent.getFirstIndex(), 1);
    harness.check(lastEvent.getLastIndex(), 3);
    
    // add a non-adjacent interval
    m.addSelectionInterval(5, 6);
    harness.check(m.isSelectedIndex(0), false);
    harness.check(m.isSelectedIndex(1), false);
    harness.check(m.isSelectedIndex(2), false);
    harness.check(m.isSelectedIndex(3), false);
    harness.check(m.isSelectedIndex(4), false);
    harness.check(m.isSelectedIndex(5), true);
    harness.check(m.isSelectedIndex(6), true);
    harness.check(m.isSelectedIndex(7), false);
    harness.check(m.getAnchorSelectionIndex(), 5);
    harness.check(m.getLeadSelectionIndex(), 6);
    harness.check(m.getMaxSelectionIndex(), 6);
    harness.check(m.getMinSelectionIndex(), 5);
    harness.check(lastEvent.getSource(), m);
    harness.check(lastEvent.getFirstIndex(), 1);
    harness.check(lastEvent.getLastIndex(), 6);
    
    // add an adjacent interval
    m.addSelectionInterval(7, 8);
    harness.check(m.isSelectedIndex(4), false);
    harness.check(m.isSelectedIndex(5), true);
    harness.check(m.isSelectedIndex(6), true);
    harness.check(m.isSelectedIndex(7), true);
    harness.check(m.isSelectedIndex(8), true);
    harness.check(m.isSelectedIndex(9), false);
    harness.check(m.getAnchorSelectionIndex(), 7);
    harness.check(m.getLeadSelectionIndex(), 8);
    harness.check(m.getMaxSelectionIndex(), 8);
    harness.check(m.getMinSelectionIndex(), 5);
    harness.check(lastEvent.getSource(), m);
    harness.check(lastEvent.getFirstIndex(), 5);
    harness.check(lastEvent.getLastIndex(), 8);
    
    // add another adjacent interval
    m.addSelectionInterval(3, 4);
    harness.check(m.isSelectedIndex(2), false);
    harness.check(m.isSelectedIndex(3), true);
    harness.check(m.isSelectedIndex(4), true);
    harness.check(m.isSelectedIndex(5), true);
    harness.check(m.isSelectedIndex(6), true);
    harness.check(m.isSelectedIndex(7), true);
    harness.check(m.isSelectedIndex(8), true);
    harness.check(m.isSelectedIndex(9), false);
    harness.check(m.getAnchorSelectionIndex(), 3);
    harness.check(m.getLeadSelectionIndex(), 4);
    harness.check(m.getMaxSelectionIndex(), 8);
    harness.check(m.getMinSelectionIndex(), 3);
    harness.check(lastEvent.getSource(), m);
    harness.check(lastEvent.getFirstIndex(), 3);
    harness.check(lastEvent.getLastIndex(), 8);
    
    m.clearSelection();
    m.addSelectionInterval(3, 1);
    harness.check(m.isSelectedIndex(0), false);
    harness.check(m.isSelectedIndex(1), true);
    harness.check(m.isSelectedIndex(2), true);
    harness.check(m.isSelectedIndex(3), true);
    harness.check(m.isSelectedIndex(4), false);
    harness.check(m.getAnchorSelectionIndex(), 3);
    harness.check(m.getLeadSelectionIndex(), 1);
    harness.check(m.getMaxSelectionIndex(), 3);
    harness.check(m.getMinSelectionIndex(), 1);
    harness.check(lastEvent.getSource(), m);
    harness.check(lastEvent.getFirstIndex(), 1);
    harness.check(lastEvent.getLastIndex(), 4);  // this is 4 because that
                                                 // is the old lead selection
  }

  private void testMultipleIntervalSelection(TestHarness harness)
  {
    harness.checkPoint("MULTIPLE_INTERVAL_SELECTION");
    DefaultListSelectionModel m = new DefaultListSelectionModel();
    m.addSelectionInterval(1, 3);
    harness.check(m.isSelectedIndex(0), false);
    harness.check(m.isSelectedIndex(1), true);
    harness.check(m.isSelectedIndex(2), true);
    harness.check(m.isSelectedIndex(3), true);
    harness.check(m.isSelectedIndex(4), false);
    
    m.clearSelection();
    m.addSelectionInterval(2, 1);
    harness.check(m.isSelectedIndex(0), false);
    harness.check(m.isSelectedIndex(1), true);
    harness.check(m.isSelectedIndex(2), true);
    harness.check(m.isSelectedIndex(3), false);
    
    // check what happens for negative indices...
    m.clearSelection();
    m.addSelectionInterval(-1, 1);
    harness.check(m.isSelectedIndex(-2), false);
    harness.check(m.isSelectedIndex(-1), false);
    harness.check(m.isSelectedIndex(0), false);
    harness.check(m.isSelectedIndex(1), false);
    harness.check(m.isSelectedIndex(2), false);
  }
}
