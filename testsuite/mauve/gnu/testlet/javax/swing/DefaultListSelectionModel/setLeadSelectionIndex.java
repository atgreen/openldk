// Tags: JDK1.2

// Copyright (C) 2005 Roman Kennke <kennke@aicas.com>
// Copyright (C) 2006 David Gilbert <david.gilbert@object-refinery.com>

// This file is part of Mauve.

// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.

// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with GNU Classpath; see the file COPYING.  If not, write to the
// Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
// 02110-1301 USA.

package gnu.testlet.javax.swing.DefaultListSelectionModel;

import javax.swing.DefaultListSelectionModel;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * This checks some functionality of the setLeadSelection method.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class setLeadSelectionIndex implements Testlet, ListSelectionListener
{
  ListSelectionEvent lastEvent;
  
  public void valueChanged(ListSelectionEvent e) 
  {
    lastEvent = e;
  }
  
  /**
   * Starts the test run.
   *
   * @param harness the test harness to use
   */
  public void test(TestHarness harness)
  {
    testSetMinusOne(harness);
    testSetPositive(harness);
    
    testSingleSelection(harness);
    testSingleIntervalSelection(harness);
    testMultipleIntervalSelection(harness);

  }

  /**
   * Checks the behaviour when trying to set the lead selection index to -1.
   * It should set the lead selection index to -1 if and only if the
   * anchor selection index is also -1.
   */
  private void testSetMinusOne(TestHarness harness)
  {
    harness.checkPoint("setMinusOne");
    DefaultListSelectionModel m = new DefaultListSelectionModel();
    // Set to some value. (works only if anchor selection index is >= 0).
    m.setAnchorSelectionIndex(1);
    m.setLeadSelectionIndex(1);
    harness.check(m.getLeadSelectionIndex(), 1);
    // This should have no effect.
    m.setLeadSelectionIndex(-1);
    harness.check(m.getLeadSelectionIndex(), 1);
    // Now set the anchor selection index to -1 and it will work with the
    // lead selection index too.
    m.setAnchorSelectionIndex(-1);
    m.setLeadSelectionIndex(-1);
    harness.check(m.getLeadSelectionIndex(), -1);
  }

  /**
   * Checks the behaviour when trying to set the lead selection index to a
   * positive integer.
   * It should set the lead selection index only if the anchor selection index
   * is also >=.
   */
  private void testSetPositive(TestHarness harness)
  {
    harness.checkPoint("setPositive");
    DefaultListSelectionModel m = new DefaultListSelectionModel();
    // The lead and anchor selection index is -1 initially.
    harness.check(m.getLeadSelectionIndex(), -1);
    harness.check(m.getAnchorSelectionIndex(), -1);
    // This will not work unless the anchor selection index is != -1
    m.setLeadSelectionIndex(0);
    harness.check(m.getLeadSelectionIndex(), -1);
    // Now set the anchor to 0 and it will also work for the lead.
    m.setAnchorSelectionIndex(0);
    m.setLeadSelectionIndex(0);
    harness.check(m.getLeadSelectionIndex(), 0);
  }
  
    private void testSingleSelection(TestHarness harness)
  {
    harness.checkPoint("SINGLE_SELECTION (1)");
    DefaultListSelectionModel m = new DefaultListSelectionModel();
    m.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    m.setLeadAnchorNotificationEnabled(true);
    m.addListSelectionListener(this);
    
    // starting from an empty selection...
    m.setLeadSelectionIndex(2);
    harness.check(m.getLeadSelectionIndex(), -1);
    harness.check(m.getAnchorSelectionIndex(), -1);
    harness.check(m.isSelectedIndex(2), false);
    harness.check(lastEvent, null);
    
    // starting from a selection
    harness.checkPoint("SINGLE_SELECTION (2)");
    m.setSelectionInterval(2, 2);
    lastEvent = null;
    m.setLeadSelectionIndex(1);
    harness.check(m.getLeadSelectionIndex(), 1);
    harness.check(m.getAnchorSelectionIndex(), 1);
    harness.check(m.isSelectedIndex(1), true);
    harness.check(m.isSelectedIndex(2), false);
    harness.check(lastEvent.getSource(), m);
    harness.check(lastEvent.getFirstIndex(), 1);
    harness.check(lastEvent.getLastIndex(), 2);
    
    // set the lead selection to -1 is ignored
    harness.checkPoint("SINGLE_SELECTION (3)");
    lastEvent = null;
    m.setLeadSelectionIndex(-1);
    harness.check(m.getLeadSelectionIndex(), 1);
    harness.check(m.getAnchorSelectionIndex(), 1);
    harness.check(m.isSelectedIndex(1), true);
    harness.check(m.isSelectedIndex(2), false);
    harness.check(lastEvent, null);
    
    // set the lead selection to -2 
    harness.checkPoint("SINGLE_SELECTION (4)");
    boolean pass = false;
    try
      {
        m.setLeadSelectionIndex(-2);
      }
    catch (IndexOutOfBoundsException e)
      {
        pass = true;  
      }
    harness.check(pass);
  }

  private void testSingleIntervalSelection(TestHarness harness)
  {
    DefaultListSelectionModel m = new DefaultListSelectionModel();
    m.setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
    m.addListSelectionListener(this);
    
    // starting from an empty selection...
    harness.checkPoint("SINGLE_INTERVAL_SELECTION (1)");
    m.setLeadSelectionIndex(2);
    harness.check(m.getLeadSelectionIndex(), -1);
    harness.check(m.getAnchorSelectionIndex(), -1);
    harness.check(m.isSelectedIndex(2), false);
    harness.check(lastEvent, null);
    
    // starting from a selection
    harness.checkPoint("SINGLE_INTERVAL_SELECTION (2)");
    m.setSelectionInterval(2, 4);
    lastEvent = null;
    m.setLeadSelectionIndex(1);
    harness.check(m.getLeadSelectionIndex(), 1);
    harness.check(m.getAnchorSelectionIndex(), 2);
    harness.check(m.isSelectedIndex(1), true);
    harness.check(m.isSelectedIndex(2), true);
    harness.check(m.isSelectedIndex(3), false);
    harness.check(m.isSelectedIndex(4), false);
    harness.check(lastEvent.getSource(), m);
    harness.check(lastEvent.getFirstIndex(), 1);
    harness.check(lastEvent.getLastIndex(), 4);

    // setting the lead selection to -1 is ignored
    harness.checkPoint("SINGLE_INTERVAL_SELECTION (3)");
    lastEvent = null;
    m.setLeadSelectionIndex(-1);
    harness.check(m.getLeadSelectionIndex(), 1);
    harness.check(m.getAnchorSelectionIndex(), 2);
    harness.check(m.isSelectedIndex(1), true);
    harness.check(m.isSelectedIndex(2), true);
    harness.check(lastEvent, null);
    
    // setting the lead selection to -2 fails
    harness.checkPoint("SINGLE_INTERVAL_SELECTION (4)");
    boolean pass = false;
    try
      {
        m.setLeadSelectionIndex(-2);
      }
    catch (IndexOutOfBoundsException e)
      {
        pass = true;
      }
    harness.check(pass);
}

  private void testMultipleIntervalSelection(TestHarness harness)
  {
    harness.checkPoint("MULTIPLE_INTERVAL_SELECTION");
    DefaultListSelectionModel m = new DefaultListSelectionModel();
    m.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);

    m.addSelectionInterval(3, 6);
    m.addListSelectionListener(this);
    m.setLeadSelectionIndex(1);
    harness.check(m.isSelectedIndex(0), false);
    harness.check(m.isSelectedIndex(1), true);
    harness.check(m.isSelectedIndex(2), true);
    harness.check(m.isSelectedIndex(3), true);
    harness.check(m.isSelectedIndex(4), false);
    harness.check(m.isSelectedIndex(5), false);
    harness.check(m.isSelectedIndex(6), false);
    harness.check(lastEvent.getSource(), m);
    harness.check(lastEvent.getFirstIndex(), 1);
    harness.check(lastEvent.getLastIndex(), 6);
  }

}
