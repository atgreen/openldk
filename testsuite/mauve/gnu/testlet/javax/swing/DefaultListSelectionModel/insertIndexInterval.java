/* insertIndexInterval.java -- some checks for the insertIndexInterval()
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

public class insertIndexInterval implements Testlet, ListSelectionListener
{
  ListSelectionEvent lastEvent;
  
  public void valueChanged(ListSelectionEvent e) 
  {
    lastEvent = e;
  }
  
  public void test(TestHarness harness)
  {
    testSingleSelection(harness);
    testSingleInterval(harness);
    testMultipleInterval(harness);
  }
  
  private void testSingleSelection(TestHarness harness)
  {
    harness.checkPoint("SINGLE_SELECTION (1)");
    DefaultListSelectionModel m = new DefaultListSelectionModel();
    m.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    m.addSelectionInterval(3, 3);
    m.addListSelectionListener(this);
    m.insertIndexInterval(3, 2, true);
    harness.check(m.isSelectedIndex(3), false);
    harness.check(m.isSelectedIndex(4), false);
    harness.check(m.isSelectedIndex(5), true);
    harness.check(m.getAnchorSelectionIndex(), 5);
    harness.check(m.getLeadSelectionIndex(), 5);
    harness.check(lastEvent.getSource(), m);
    harness.check(lastEvent.getFirstIndex(), 3);
    harness.check(lastEvent.getLastIndex(), 5);
    
    harness.checkPoint("SINGLE_SELECTION (2)");
    lastEvent = null;
    m.insertIndexInterval(5, 2, false);  // this does nothing
    harness.check(m.isSelectedIndex(5), true);
    harness.check(m.isSelectedIndex(6), false);
    harness.check(m.isSelectedIndex(7), false);
    harness.check(lastEvent, null);
    
    // try negative index
    harness.checkPoint("SINGLE_SELECTION (3)");
    boolean pass = false;
    try
    {
      m.insertIndexInterval(-1, 1, true);
    }
    catch (IndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // try negative count
    harness.checkPoint("SINGLE_SELECTION (4)");
    pass = false;
    try
    {
      m.insertIndexInterval(0, -1, true);
    }
    catch (IndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);  
    
    // try zero count
    harness.checkPoint("SINGLE_SELECTION (5)");
    lastEvent = null;
    m.insertIndexInterval(0, 0, true);
    harness.check(lastEvent.getFirstIndex(), 0);  
    harness.check(lastEvent.getLastIndex(), 6);  
  }

  private void testSingleInterval(TestHarness harness)
  {
    harness.checkPoint("SINGLE_INTERVAL_SELECTION (1)");
    DefaultListSelectionModel m = new DefaultListSelectionModel();
    m.setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
    m.setSelectionInterval(3, 3);
    m.addListSelectionListener(this);
    
    // here, we insert two new items in the list right before the selected item
    // 3 (which moves up to position 5).
    m.insertIndexInterval(3, 2, true);
    harness.check(m.isSelectedIndex(3), true); // FIXME: surely wrong?
    harness.check(m.isSelectedIndex(4), true); // FIXME: likewise?
    harness.check(m.isSelectedIndex(5), true);
    harness.check(m.isSelectedIndex(6), false);
    harness.check(m.getAnchorSelectionIndex(), 5);
    harness.check(m.getLeadSelectionIndex(), 5);
    harness.check(lastEvent.getSource(), m);
    harness.check(lastEvent.getFirstIndex(), 3);
    harness.check(lastEvent.getLastIndex(), 5);

    harness.checkPoint("SINGLE_INTERVAL_SELECTION (2)");
    m = new DefaultListSelectionModel();
    m.setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
    m.setSelectionInterval(3, 3);
    m.addListSelectionListener(this);
    lastEvent = null;
    
    // here, we insert two new items in the list right AFTER the selected item
    // 3 
    m.insertIndexInterval(3, 2, false);
    harness.check(m.isSelectedIndex(3), true); 
    harness.check(m.isSelectedIndex(4), true); // FIXME: surely wrong?
    harness.check(m.isSelectedIndex(5), true); // FIXME: likewise?
    harness.check(m.isSelectedIndex(6), false);
    harness.check(m.getAnchorSelectionIndex(), 3);
    harness.check(m.getLeadSelectionIndex(), 3);
    harness.check(lastEvent.getSource(), m);
    harness.check(lastEvent.getFirstIndex(), 4);
    harness.check(lastEvent.getLastIndex(), 5);
  
    // try negative index
    boolean pass = false;
    try
    {
      m.insertIndexInterval(-1, 1, true);
    }
    catch (IndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // try negative count
    pass = false;
    try
    {
      m.insertIndexInterval(0, -1, true);
    }
    catch (IndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // try zero count
    lastEvent = null;
    m.insertIndexInterval(0, 0, true);
    harness.check(lastEvent.getSource(), m);
    harness.check(lastEvent.getFirstIndex(), 0);
    harness.check(lastEvent.getLastIndex(), 6);
  }
  
  private void testMultipleInterval(TestHarness harness)
  {
    harness.checkPoint("MULTIPLE_INTERVAL_SELECTION (1)");
    DefaultListSelectionModel m = new DefaultListSelectionModel();
    m.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
    m.addSelectionInterval(1, 1);
    m.addSelectionInterval(7, 7);
    m.addSelectionInterval(3, 5);
    m.addListSelectionListener(this);
    m.insertIndexInterval(2, 2, true);
    harness.check(m.isSelectedIndex(0), false);
    harness.check(m.isSelectedIndex(1), true);
    harness.check(m.isSelectedIndex(2), false);
    harness.check(m.isSelectedIndex(3), false);
    harness.check(m.isSelectedIndex(4), false);
    harness.check(m.isSelectedIndex(5), true);
    harness.check(m.isSelectedIndex(6), true);
    harness.check(m.isSelectedIndex(7), true);
    harness.check(m.isSelectedIndex(8), false);
    harness.check(m.isSelectedIndex(9), true);
    harness.check(m.isSelectedIndex(10), false);
    harness.check(m.getAnchorSelectionIndex(), 5);
    harness.check(m.getLeadSelectionIndex(), 7);
    harness.check(lastEvent.getSource(), m);
    harness.check(lastEvent.getFirstIndex(), 3);
    harness.check(lastEvent.getLastIndex(), 9);
    
    m.insertIndexInterval(1, 2, false);
    harness.check(m.isSelectedIndex(0), false);
    harness.check(m.isSelectedIndex(1), true);
    harness.check(m.isSelectedIndex(2), true);
    harness.check(m.isSelectedIndex(3), true);
    harness.check(m.isSelectedIndex(4), false);
    harness.check(m.isSelectedIndex(5), false);
    harness.check(m.isSelectedIndex(6), false);
    harness.check(m.isSelectedIndex(7), true);
    harness.check(m.isSelectedIndex(8), true);
    harness.check(m.isSelectedIndex(9), true);
    harness.check(m.isSelectedIndex(10), false);
    harness.check(m.isSelectedIndex(11), true);
    harness.check(m.isSelectedIndex(12), false);
    harness.check(m.getAnchorSelectionIndex(), 7);
    harness.check(m.getLeadSelectionIndex(), 9);
    harness.check(lastEvent.getSource(), m);
    harness.check(lastEvent.getFirstIndex(), 2);
    harness.check(lastEvent.getLastIndex(), 11);  
  }
}
