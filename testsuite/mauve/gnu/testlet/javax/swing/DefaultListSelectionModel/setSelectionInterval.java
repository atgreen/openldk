/* setSelectionInterval.java -- some checks for the setSelectionInterval()
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

import java.util.List;

import javax.swing.DefaultListSelectionModel;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

public class setSelectionInterval implements Testlet, ListSelectionListener
{
  List events = new java.util.ArrayList();
  
  public void valueChanged(ListSelectionEvent e) 
  {
    events.add(e);
  }

  public void test(TestHarness harness)
  {
    testSingle(harness);
    testSingleInterval(harness);
    testMultipleInterval(harness);
    testSingleX(harness);
  }
  
  private void testSingle(TestHarness harness)
  {
    harness.checkPoint("SINGLE_SELECTION");
    DefaultListSelectionModel m = new DefaultListSelectionModel();
    m.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    m.addListSelectionListener(this);
    
    harness.checkPoint("SINGLE_SELECTION (1)");
    m.setSelectionInterval(6, 7);
    harness.check(m.isSelectedIndex(6), false);
    harness.check(m.isSelectedIndex(7), true);
    harness.check(m.getAnchorSelectionIndex(), 7);
    harness.check(m.getLeadSelectionIndex(), 7);
    ListSelectionEvent lastEvent = (ListSelectionEvent) events.get(0);
    harness.check(lastEvent.getSource(), m);
    harness.check(lastEvent.getFirstIndex(), 7);
    harness.check(lastEvent.getLastIndex(), 7);
    
    // no event is generated if we update the same again
    events.clear();
    harness.checkPoint("SINGLE_SELECTION (2)");
    m.setSelectionInterval(6, 7);
    harness.check(events.size(), 0);
        
    // now if we set another selection, the event range should cover the old
    // index too
    events.clear();
    harness.checkPoint("SINGLE_SELECTION (3)");
    m.setSelectionInterval(3, 3);
    harness.check(m.isSelectedIndex(3), true);
    harness.check(m.isSelectedIndex(7), false);
    lastEvent = (ListSelectionEvent) events.get(0);
    harness.check(lastEvent.getSource(), m);
    harness.check(lastEvent.getFirstIndex(), 3);
    harness.check(lastEvent.getLastIndex(), 7);
    
    // the anchor can move around independently of the selection, is it 
    // included in the event range? YES
    harness.checkPoint("SINGLE_SELECTION (4)");
    m.setAnchorSelectionIndex(5);
    events.clear();
    m.setSelectionInterval(4, 4);
    lastEvent = (ListSelectionEvent) events.get(0);
    harness.check(lastEvent.getSource(), m);
    harness.check(lastEvent.getFirstIndex(), 3);
    harness.check(lastEvent.getLastIndex(), 5);
    
    // try -2 for the initial index
    harness.checkPoint("SINGLE_SELECTION (5)");
    m = new DefaultListSelectionModel();
    m.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    m.setSelectionInterval(2, 2);
    m.addListSelectionListener(this);
    events.clear();
    m.setSelectionInterval(-2, 0);
    lastEvent = (ListSelectionEvent) events.get(0);
    harness.check(lastEvent.getSource(), m);
    harness.check(lastEvent.getFirstIndex(), 0);
    harness.check(lastEvent.getLastIndex(), 2);
    harness.check(m.getLeadSelectionIndex(), 0);
    harness.check(m.getAnchorSelectionIndex(), 0);

    // try -1 for the initial index
    harness.checkPoint("SINGLE_SELECTION (6)");
    m = new DefaultListSelectionModel();
    m.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    m.setSelectionInterval(2, 2);
    m.addListSelectionListener(this);
    events.clear();
    m.setSelectionInterval(-1, 0);
    harness.check(events.size(), 0);
  
    // try -2 for the second index
    harness.checkPoint("SINGLE_SELECTION (7)");
    m = new DefaultListSelectionModel();
    m.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    m.setSelectionInterval(2, 2);
    m.addListSelectionListener(this);
    events.clear();
    boolean pass = false;
    try
    {
      m.setSelectionInterval(0, -2);
    }
    catch (IndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);

    // try -1 for the second index
    harness.checkPoint("SINGLE_SELECTION (8)");
    m = new DefaultListSelectionModel();
    m.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    m.setSelectionInterval(2, 2);
    m.addListSelectionListener(this);
    events.clear();
    m.setSelectionInterval(0, -1);
    harness.check(events.size(), 0);
  }
  
  /**
   * Some checks for a model using SINGLE_INTERVAL_SELECTION.
   * 
   * @param harness  the test harness.
   */
  private void testSingleInterval(TestHarness harness)
  {
    harness.checkPoint("SINGLE_INTERVAL_SELECTION (1)");
    DefaultListSelectionModel m = new DefaultListSelectionModel();
    m.setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
    events.clear();
    m.addListSelectionListener(this);
    m.setSelectionInterval(6, 7);
    harness.check(m.isSelectedIndex(5), false); 
    harness.check(m.isSelectedIndex(6), true); 
    harness.check(m.isSelectedIndex(7), true); 
    harness.check(m.isSelectedIndex(8), false);
    harness.check(m.getAnchorSelectionIndex(), 6);
    harness.check(m.getLeadSelectionIndex(), 7);
    harness.check(events.size(), 1);
    ListSelectionEvent lastEvent = (ListSelectionEvent) events.get(0);
    harness.check(lastEvent.getSource(), m);
    harness.check(lastEvent.getFirstIndex(), 6);
    harness.check(lastEvent.getLastIndex(), 7);

    // no event is generated if we update the same again
    harness.checkPoint("SINGLE_INTERVAL_SELECTION (2)");    
    events.clear();
    m.setSelectionInterval(6, 7);
    harness.check(events.size(), 0);

    // now if we set another selection, the event range should cover the old
    // index too
    harness.checkPoint("SINGLE_INTERVAL_SELECTION (3)");    
    events.clear();
    m.setSelectionInterval(3, 3);
    harness.check(m.isSelectedIndex(3), true);
    harness.check(m.isSelectedIndex(6), false);
    harness.check(m.isSelectedIndex(7), false);
    harness.check(events.size(), 1);
    lastEvent = (ListSelectionEvent) events.get(0);
    harness.check(lastEvent.getSource(), m);
    harness.check(lastEvent.getFirstIndex(), 3);
    harness.check(lastEvent.getLastIndex(), 7);
    
    // the anchor can move around independently of the selection, is it 
    // included in the event range? YES
    harness.checkPoint("SINGLE_INTERVAL_SELECTION (3)");    
    m.setAnchorSelectionIndex(5);
    events.clear();
    m.setSelectionInterval(4, 4);
    harness.check(m.isSelectedIndex(3), false);
    harness.check(m.isSelectedIndex(4), true);
    harness.check(m.isSelectedIndex(5), false);
    lastEvent = (ListSelectionEvent) events.get(0);
    harness.check(lastEvent.getSource(), m);
    harness.check(lastEvent.getFirstIndex(), 3);
    harness.check(lastEvent.getLastIndex(), 5);
  }
  
  private void testMultipleInterval(TestHarness harness)
  {
    harness.checkPoint("MULTIPLE_INTERVAL_SELECTION");
    DefaultListSelectionModel m = new DefaultListSelectionModel();
    m.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
    events.clear();
    m.addListSelectionListener(this);
    m.setSelectionInterval(3, 5);
    harness.check(m.isSelectedIndex(2), false);
    harness.check(m.isSelectedIndex(3), true);
    harness.check(m.isSelectedIndex(4), true);
    harness.check(m.isSelectedIndex(5), true);
    harness.check(m.isSelectedIndex(6), false);
    harness.check(m.getAnchorSelectionIndex(), 3);
    harness.check(m.getLeadSelectionIndex(), 5);
    harness.check(m.getMinSelectionIndex(), 3);
    harness.check(m.getMaxSelectionIndex(), 5);
    harness.check(events.size(), 1);
    ListSelectionEvent lastEvent = (ListSelectionEvent) events.get(0);
    harness.check(lastEvent.getSource(), m);
    harness.check(lastEvent.getFirstIndex(), 3);
    harness.check(lastEvent.getLastIndex(), 5);
    events.clear();
    
    m.setSelectionInterval(2, 3);
    harness.check(m.isSelectedIndex(2), true);
    harness.check(m.isSelectedIndex(3), true);
    harness.check(m.isSelectedIndex(4), false);
    harness.check(m.isSelectedIndex(5), false);
    harness.check(m.isSelectedIndex(6), false);
    harness.check(m.getAnchorSelectionIndex(), 2);
    harness.check(m.getLeadSelectionIndex(), 3);
    harness.check(m.getMinSelectionIndex(), 2);
    harness.check(m.getMaxSelectionIndex(), 3);
    lastEvent = (ListSelectionEvent) events.get(0);
    harness.check(lastEvent.getSource(), m);
    harness.check(lastEvent.getFirstIndex(), 2);
    harness.check(lastEvent.getLastIndex(), 5);
  }
  
  /** 
   * A special case where the model is SINGLE_SELECTION, but this mode is set 
   * AFTER a range of items is selected.
   * 
   * @param harness
   */
  private void testSingleX(TestHarness harness)
  {
    harness.checkPoint("SINGLE_SELECTION_X");
    DefaultListSelectionModel m = new DefaultListSelectionModel();
    m.addSelectionInterval(2, 4);
    m.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    events.clear();
    m.addListSelectionListener(this);
    m.setSelectionInterval(0, 1);
    harness.check(m.isSelectedIndex(0), false);
    harness.check(m.isSelectedIndex(1), true);
    harness.check(m.getAnchorSelectionIndex(), 1);
    harness.check(m.getLeadSelectionIndex(), 1);
    harness.check(events.size(), 1);
    ListSelectionEvent lastEvent = (ListSelectionEvent) events.get(0);
    harness.check(lastEvent.getSource(), m);
    harness.check(lastEvent.getFirstIndex(), 1);
    harness.check(lastEvent.getLastIndex(), 4);
  }
  
}
