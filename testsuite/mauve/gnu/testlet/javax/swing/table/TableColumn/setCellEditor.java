/* setCellEditor.java -- some checks for the setCellEditor() method in the
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

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.List;

import javax.swing.DefaultCellEditor;
import javax.swing.JTextField;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableColumn;

public class setCellEditor implements Testlet, PropertyChangeListener
{
  List events = new java.util.ArrayList();
  
  public void propertyChange(PropertyChangeEvent e)
  {
    events.add(e);
  }
  
  public void test(TestHarness harness)
  {
    TableColumn c = new TableColumn();
    harness.check(c.getCellEditor(), null);
    c.addPropertyChangeListener(this);
    TableCellEditor editor = new DefaultCellEditor(new JTextField());
    c.setCellEditor(editor);
    harness.check(c.getCellEditor(), editor);
    PropertyChangeEvent e = (PropertyChangeEvent) events.get(0);
    harness.check(e.getPropertyName(), "cellEditor");
    harness.check(e.getOldValue(), null);
    harness.check(e.getNewValue(), editor);
    
    // set to null
    events.clear();
    c.setCellEditor(null);
    e = (PropertyChangeEvent) events.get(0);
    harness.check(e.getPropertyName(), "cellEditor");
    harness.check(e.getOldValue(), editor);
    harness.check(e.getNewValue(), null);
  }
}
