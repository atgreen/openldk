// Tags: JDK1.2

// Uses: TestList

// Copyright (C) 2005 Roman Kennke <kennke@aicas.com>

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
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

package gnu.testlet.javax.swing.JList.AccessibleJList;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Vector;

import javax.accessibility.AccessibleContext;
import javax.swing.event.ListDataEvent;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Checks the functionality of the method contentsChanged in
 * javax.swing.JList.AccessibleJList. This method fires two
 * PropertyChangeEvents, one with
 * AccessibleContext.ACCESSIBLE_VISIBLE_DATA_PROPERTY and one with
 * AccessibleContext.ACCESSIBLE_SELECTION_PROPERTY.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class contentsChanged implements Testlet, PropertyChangeListener
{
  Vector receivedEvents = new Vector();

  public void test(TestHarness harness)
  {
    TestList l = new TestList();
    TestList.AccessibleTestList al =
      (TestList.AccessibleTestList) l.getAccessibleContext();
    al.addPropertyChangeListener(this);

    ListDataEvent ev = new ListDataEvent(l, ListDataEvent.INTERVAL_ADDED, 1, 2);
    receivedEvents.clear();
    al.contentsChanged(ev);
    harness.check(receivedEvents.size(), 1);
    PropertyChangeEvent ev1 = (PropertyChangeEvent) receivedEvents.get(0);
    harness.check(ev1.getPropertyName(),
                  AccessibleContext.ACCESSIBLE_VISIBLE_DATA_PROPERTY);
    harness.check(ev1.getSource(), al);
    harness.check(ev1.getOldValue(), Boolean.FALSE);
    harness.check(ev1.getNewValue(), Boolean.TRUE);
  }

  public void propertyChange(PropertyChangeEvent e) {
    receivedEvents.add(e);
  }
}
