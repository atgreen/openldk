/* removeIndexInterval.java -- some checks for the removeIndexInterval()
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

public class removeIndexInterval implements Testlet, ListSelectionListener
{
  private ListSelectionEvent lastEvent;
   
  public void valueChanged(ListSelectionEvent event) 
  {
    lastEvent = event;
  }

  public void test(TestHarness harness)
  {
    testSingleSelection(harness);
    testSingleInterval(harness);
    testMultipleInterval(harness);
  }
  
  public void testSingleSelection(TestHarness harness)
  {
    harness.checkPoint("SINGLE_SELECTION");
    DefaultListSelectionModel m = new DefaultListSelectionModel();
    m.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    
    // check 1 - the selected item is in the removed range
    m.addSelectionInterval(6, 7);
    harness.check(m.isSelectedIndex(6), false);
    harness.check(m.isSelectedIndex(7), true);
    harness.check(m.getAnchorSelectionIndex(), 7);
    harness.check(m.getLeadSelectionIndex(), 7);
    m.addListSelectionListener(this);
    m.removeIndexInterval(5, 7);
    harness.check(m.isSelectionEmpty(), true);
    harness.check(m.getAnchorSelectionIndex(), 4);
    harness.check(m.getLeadSelectionIndex(), 4);
    
    harness.check(lastEvent.getSource(), m);
    harness.check(lastEvent.getFirstIndex(), 4);
    harness.check(lastEvent.getLastIndex(), 7); 
    
    // check 2 - the selected item is below the removed range
    m.setSelectionInterval(1, 1);
    lastEvent = null;
    m.removeIndexInterval(2, 4);
    harness.check(m.isSelectedIndex(1), true);
    harness.check(m.getAnchorSelectionIndex(), 1);
    harness.check(m.getLeadSelectionIndex(), 1);
    harness.check(lastEvent, null); // no event, because nothing changed
    
    // check 3 - the selected item is above the removed range
    m.setSelectionInterval(5, 5);
    lastEvent = null;
    m.removeIndexInterval(2, 4);
    harness.check(m.isSelectedIndex(2), true);
    harness.check(m.getAnchorSelectionIndex(), 2);
    harness.check(m.getLeadSelectionIndex(), 2);
    harness.check(lastEvent.getFirstIndex(), 2); 
    harness.check(lastEvent.getLastIndex(), 5);
  }

  public void testSingleInterval(TestHarness harness)
  {
    harness.checkPoint("SINGLE_INTERVAL_SELECTION");
    DefaultListSelectionModel m = new DefaultListSelectionModel();
    m.setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
    
    // check 1 - remove the middle of a selection interval
    m.addSelectionInterval(2, 6);
    harness.check(m.isSelectedIndex(1), false);
    harness.check(m.isSelectedIndex(2), true);
    harness.check(m.isSelectedIndex(3), true);
    harness.check(m.isSelectedIndex(4), true);
    harness.check(m.isSelectedIndex(5), true);
    harness.check(m.isSelectedIndex(6), true);
    harness.check(m.isSelectedIndex(7), false);
    m.addListSelectionListener(this);
    m.removeIndexInterval(3, 5);
    harness.check(m.isSelectedIndex(1), false);
    harness.check(m.isSelectedIndex(2), true);
    harness.check(m.isSelectedIndex(3), true);
    harness.check(m.isSelectedIndex(4), false);
    harness.check(m.isSelectedIndex(5), false);
    harness.check(m.isSelectedIndex(6), false);
    harness.check(m.isSelectedIndex(7), false);
    harness.check(m.getLeadSelectionIndex(), 3);
    harness.check(m.getAnchorSelectionIndex(), 2);
    
    harness.check(lastEvent.getSource(), m);
    harness.check(lastEvent.getFirstIndex(), 3);
    harness.check(lastEvent.getLastIndex(), 6); 
  }

  public void testMultipleInterval(TestHarness harness)
  {
    harness.checkPoint("MULTIPLE_INTERVAL_SELECTION");
    DefaultListSelectionModel m = new DefaultListSelectionModel();
    m.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
    m.addSelectionInterval(2, 6);
    harness.check(m.isSelectedIndex(1), false);
    harness.check(m.isSelectedIndex(2), true);
    harness.check(m.isSelectedIndex(3), true);
    harness.check(m.isSelectedIndex(4), true);
    harness.check(m.isSelectedIndex(5), true);
    harness.check(m.isSelectedIndex(6), true);
    harness.check(m.isSelectedIndex(7), false);
    m.addListSelectionListener(this);
    m.removeIndexInterval(3, 5);
    harness.check(m.isSelectedIndex(1), false);
    harness.check(m.isSelectedIndex(2), true);
    harness.check(m.isSelectedIndex(3), true);
    harness.check(m.isSelectedIndex(4), false);
    harness.check(m.isSelectedIndex(5), false);
    harness.check(m.isSelectedIndex(6), false);
    harness.check(m.isSelectedIndex(7), false);
    
    harness.check(lastEvent.getSource(), m);
    harness.check(lastEvent.getFirstIndex(), 3);
    harness.check(lastEvent.getLastIndex(), 6); 
  }

}
