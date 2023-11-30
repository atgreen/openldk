// Tags: JDK1.2

// Copyright (C) 2005 Roman Kennke <kennke@aicas.com>

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

package gnu.testlet.javax.swing.table.TableColumn;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.table.TableColumn;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Tests if the properties of the class TableColumn are correctly firing
 * events.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class properties implements Testlet
{

  Object changedProperty;

  class TestPropertyListener implements PropertyChangeListener
  {
    public void propertyChange(PropertyChangeEvent e)
    {
      changedProperty = e.getPropertyName();
    }
  }

  public void test(TestHarness harness)
  {
    TableColumn tc = new TableColumn();
    tc.addPropertyChangeListener(new TestPropertyListener());

    tc.setPreferredWidth(100);
    changedProperty = null;
    tc.setPreferredWidth(200);
    harness.check(changedProperty, "preferredWidth", "preferredWidth");

    tc.setWidth(100);
    changedProperty = null;
    tc.setWidth(200);
    harness.check(changedProperty, "width", "width");
  
  }

}
