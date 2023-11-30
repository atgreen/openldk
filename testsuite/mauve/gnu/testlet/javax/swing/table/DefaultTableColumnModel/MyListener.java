// Tags: not-a-test

// Copyright (C) 2005 David Gilbert <david.gilbert@object-refinery.com>

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
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.javax.swing.table.DefaultTableColumnModel;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.event.ChangeEvent;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.TableColumnModelEvent;
import javax.swing.event.TableColumnModelListener;

/**
 * A support class for testing.
 */
public class MyListener implements TableColumnModelListener,
                                   PropertyChangeListener
{

  // references to events received
  private TableColumnModelEvent event;
  private ChangeEvent changeEvent;
  private ListSelectionEvent selectionEvent;

  public MyListener() 
  {
    clearEvents();
  }
  
  /**
   * Clear event references ready for new test.
   */
  public void clearEvents()
  {
    this.event = null;
    this.changeEvent = null;
    this.selectionEvent = null;
  }
  
  public TableColumnModelEvent getEvent() 
  {
    return this.event; 
  }

  public ChangeEvent getChangeEvent() 
  {
    return this.changeEvent; 
  }
  
  public ListSelectionEvent getSelectionEvent()
  {
    return this.selectionEvent; 
  }
  
  public void columnAdded(TableColumnModelEvent event) 
  {
    this.event = event;  
  }

  public void columnMarginChanged(ChangeEvent event) 
  {
    this.changeEvent = event; 
  }

  public void columnMoved(TableColumnModelEvent event)  
  {
    this.event = event; 
  }

  public void columnRemoved(TableColumnModelEvent event) 
  {
    this.event = event; 
  }

  public void columnSelectionChanged(ListSelectionEvent event)
  {
    this.selectionEvent = event; 
  }

  public void propertyChange(PropertyChangeEvent evt)
  {
    System.out.println(evt.toString()); 
  }
}
