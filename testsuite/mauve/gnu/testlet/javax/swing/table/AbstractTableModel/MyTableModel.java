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

package gnu.testlet.javax.swing.table.AbstractTableModel;

import javax.swing.table.AbstractTableModel;

/**
 * This class provides a default implementation that can be used to test
 * the methods in AbstractTableModel.
 */
public class MyTableModel extends AbstractTableModel {

  public MyTableModel()
  {
  }
  
  public int getRowCount() 
  {
    return 1; 
  }
  
  public int getColumnCount() 
  {
    return 3;  
  }
 
  public Object getValueAt(int row, int column) 
  {
    return new Integer(column); 
  }
  
}
