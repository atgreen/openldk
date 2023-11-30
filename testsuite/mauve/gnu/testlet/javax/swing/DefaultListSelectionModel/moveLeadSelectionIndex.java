/* moveLeadSelectionIndex.java -- some checks for the moveLeadSelectionIndex()
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
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

public class moveLeadSelectionIndex implements Testlet, ListSelectionListener {
    
  ListSelectionEvent lastEvent;
  
  public void valueChanged(ListSelectionEvent e) 
  {
    lastEvent = e;
  }

  public void test(TestHarness harness)
  {
    DefaultListSelectionModel m = new DefaultListSelectionModel();
    m.addSelectionInterval(3, 5);
    m.addListSelectionListener(this);
    m.moveLeadSelectionIndex(7);
    harness.check(m.isSelectedIndex(5), true);
    harness.check(m.isSelectedIndex(6), false);
    harness.check(m.isSelectedIndex(7), false);
    harness.check(lastEvent.getSource(), m);
    harness.check(lastEvent.getFirstIndex(), 5);
    harness.check(lastEvent.getLastIndex(), 7);
  }

}
