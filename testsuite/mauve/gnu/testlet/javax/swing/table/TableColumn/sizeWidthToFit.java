/* sizeWidthToFit.java -- Some checks for the sizeWidthToFit() method in the
       TableColumn class.
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

// Tags: JDK1.2

package gnu.testlet.javax.swing.table.TableColumn;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Dimension;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.List;

import javax.swing.JComponent;
import javax.swing.JTable;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;

public class sizeWidthToFit implements Testlet, PropertyChangeListener
{
  List events = new java.util.ArrayList();
  
  public void propertyChange(PropertyChangeEvent event)
  {
    events.add(event);
  }
  
  public void test(TestHarness harness)
  {
    TableColumn c = new TableColumn();
    harness.check(c.getHeaderRenderer(), null);
    c.addPropertyChangeListener(this);
    
    // with no header renderer, the method should do nothing
    c.sizeWidthToFit();
    harness.check(c.getMinWidth(), 15);
    harness.check(c.getMaxWidth(), Integer.MAX_VALUE);
    harness.check(c.getPreferredWidth(), 75);
    harness.check(c.getWidth(), 75);
    
    JTable t = new JTable();
    JTableHeader h = t.getTableHeader();
    JComponent r = (JComponent) h.getDefaultRenderer();
    r.setMinimumSize(new Dimension(13, 5));
    r.setMaximumSize(new Dimension(999, 99));
    r.setPreferredSize(new Dimension(34, 21));
    c.setHeaderRenderer((TableCellRenderer) r);
    harness.check(c.getMinWidth(), 15);
    harness.check(c.getMaxWidth(), Integer.MAX_VALUE);
    harness.check(c.getPreferredWidth(), 75);
    harness.check(c.getWidth(), 75);
    c.sizeWidthToFit();    
    harness.check(c.getMinWidth(), 13);
    harness.check(c.getMaxWidth(), 999);
    harness.check(c.getPreferredWidth(), 34);
    harness.check(c.getWidth(), 34);
    
    PropertyChangeEvent e0 = (PropertyChangeEvent) events.get(0);
    harness.check(e0.getPropertyName(), "headerRenderer");
    harness.check(e0.getOldValue(), null);
    harness.check(e0.getNewValue(), r);
    PropertyChangeEvent e1 = (PropertyChangeEvent) events.get(1);
    harness.check(e1.getPropertyName(), "minWidth");
    harness.check(e1.getOldValue(), new Integer(15));
    harness.check(e1.getNewValue(), new Integer(13));
    PropertyChangeEvent e2 = (PropertyChangeEvent) events.get(2);
    harness.check(e2.getPropertyName(), "maxWidth");
    harness.check(e2.getOldValue(), new Integer(Integer.MAX_VALUE));
    harness.check(e2.getNewValue(), new Integer(999));
    PropertyChangeEvent e3 = (PropertyChangeEvent) events.get(3);
    harness.check(e3.getPropertyName(), "preferredWidth");
    harness.check(e3.getOldValue(), new Integer(75));
    harness.check(e3.getNewValue(), new Integer(34));
    PropertyChangeEvent e4 = (PropertyChangeEvent) events.get(4);
    harness.check(e4.getPropertyName(), "width");
    harness.check(e4.getOldValue(), new Integer(75));
    harness.check(e4.getNewValue(), new Integer(34));
  }
}
