/* setSelectionMode.java -- some checks for the setSelectionMode()
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

public class setSelectionMode implements Testlet, ListSelectionListener
{
  ListSelectionEvent lastEvent;
  
  public void valueChanged(ListSelectionEvent event) {
    lastEvent = event;
  }

  public void test(TestHarness harness)
  {
    testGeneral(harness);  
    testIntervalToSingle(harness);
  }
  
  private void testGeneral(TestHarness harness)
  {
    harness.checkPoint("testGeneral()");
    DefaultListSelectionModel m = new DefaultListSelectionModel();
    m.addListSelectionListener(this);
    harness.check(m.getSelectionMode(), 
            ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
    m.setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
    harness.check(m.getSelectionMode(), 
            ListSelectionModel.SINGLE_INTERVAL_SELECTION);
    harness.check(lastEvent, null);
    
    // try bad value
    boolean pass = false;
    try
      {
        m.setSelectionMode(99);
      }
    catch (IllegalArgumentException e)
      {
        pass = true;  
      }
    harness.check(pass);
  }
  
  private void testIntervalToSingle(TestHarness harness)
  {
    harness.checkPoint("testIntervalToSingle()");
    DefaultListSelectionModel m = new DefaultListSelectionModel();
    m.setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
    m.addSelectionInterval(2, 4);
    m.addListSelectionListener(this);
    m.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    harness.check(m.getSelectionMode(), ListSelectionModel.SINGLE_SELECTION);
    harness.check(m.isSelectedIndex(2), true);
    harness.check(m.isSelectedIndex(3), true);
    harness.check(m.isSelectedIndex(4), true);
    harness.check(lastEvent, null);
  }
}
