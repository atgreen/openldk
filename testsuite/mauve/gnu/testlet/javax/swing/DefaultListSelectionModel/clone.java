/* clone.java -- some checks for the clone()
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

public class clone implements Testlet, ListSelectionListener
{
  public void valueChanged(ListSelectionEvent e) 
  {
    // ignore
  }

  public void test(TestHarness harness)
  {
    DefaultListSelectionModel m1 = new DefaultListSelectionModel();
    m1.setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
    m1.setLeadAnchorNotificationEnabled(false);
    m1.addListSelectionListener(this);
    m1.addSelectionInterval(5, 9);
    
    DefaultListSelectionModel m2 = null;
    try
      {
        m2 = (DefaultListSelectionModel) m1.clone();
      }
    catch (CloneNotSupportedException e)
      {
        
      }
    harness.check(m2.getSelectionMode(), 
            ListSelectionModel.SINGLE_INTERVAL_SELECTION);
    harness.check(m2.isLeadAnchorNotificationEnabled(), false);
    harness.check(m2.isSelectedIndex(4), false);
    harness.check(m2.isSelectedIndex(5), true);
    harness.check(m2.isSelectedIndex(9), true);
    harness.check(m2.isSelectedIndex(10), false);
    
    // confirm that m1 and m2 are independent
    m2.clearSelection();
    harness.check(m2.isSelectionEmpty(), true);
    harness.check(m1.isSelectionEmpty(), false);
    
    // confirm that m2 doesn't have any listeners
    ListSelectionListener[] listeners = m2.getListSelectionListeners();
    harness.check(listeners.length, 0);
    listeners = m1.getListSelectionListeners();
    harness.check(listeners.length, 1);    
  }
}
