/* firePropertyChange.java -- Tests firePropertyChange()
   Copyright (C) 2006 Roman Kennke (kennke@aicas.com)
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

package gnu.testlet.java.beans.PropertyChangeSupport;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.ArrayList;

import junit.framework.TestCase;

public class firePropertyChange extends TestCase
  implements PropertyChangeListener
{
  /**
   * The object to test.
   */
  private PropertyChangeSupport change;

  /**
   * The received events.
   */
  private ArrayList events;

  public void setUp()
  {
    change = new PropertyChangeSupport(this);
    change.addPropertyChangeListener(this);
    events = new ArrayList();
  }

  public void tearDown()
  {
    change = null;
    events = null;
  }

  public void testNullNull()
  {
    change.firePropertyChange("test", null, null);
    assertEquals(events.size(), 1);
    PropertyChangeEvent ev = (PropertyChangeEvent) events.get(0);
    assertEquals(ev.getPropertyName(), "test");
    assertNull(ev.getNewValue());
    assertNull(ev.getOldValue());
  }

  public void propertyChange(PropertyChangeEvent e)
  {
    events.add(e);
  }
}
